;;; cperl-mode.el --- Perl code editing commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985-2026 Free Software Foundation, Inc.

;; Author: Ilya Zakharevich <ilyaz@cpan.org>
;;	Bob Olson
;;	Jonathan Rockway <jon@jrock.us>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: languages, Perl
;; Package-Requires: ((emacs "26.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; You can either fine-tune the bells and whistles of this mode or
;; bulk enable them by putting this in your Init file:

;;     (setq cperl-hairy t)

;; DO NOT FORGET to read micro-docs (available from `Perl' menu)   <<<<<<
;; or as help on variables `cperl-tips', `cperl-praise',           <<<<<<
;; `cperl-speed'.                                                  <<<<<<
;;
;; Or search for "Short extra-docs" further down in this file for
;; details on how to use `cperl-mode' instead of `perl-mode' and lots
;; of other details.

;; The mode information (on C-h m) provides some customization help.

;; Faces used: three faces for first-class and second-class keywords
;; and control flow words, one for each: comments, string, labels,
;; functions definitions and packages, arrays, hashes, and variable
;; definitions.

;; This mode supports imenu.  You can use imenu from the keyboard
;; (M-g i), but you might prefer binding it like this:
;;
;;     (define-key global-map [M-S-down-mouse-3] #'imenu)

;; This version supports the syntax added by the MooseX::Declare CPAN
;; module, as well as Perl 5.10 keywords.

;;; Code:

;;; Compatibility with older versions (for publishing on ELPA)
;; The following helpers allow cperl-mode.el to work with older
;; versions of Emacs.
;;
;; Whenever the minimum version is bumped (see "Package-Requires"
;; above), please eliminate the corresponding compatibility-helpers.
;; Whenever you create a new compatibility-helper, please add it here.

;; Available in Emacs 27.1: time-convert
(defalias 'cperl--time-convert
  (if (fboundp 'time-convert) 'time-convert
    'encode-time))

;; Available in Emacs 28: format-prompt
(defalias 'cperl--format-prompt
  (if (fboundp 'format-prompt) 'format-prompt
    (lambda (msg default)
      (if default (format "%s (default %s): " msg default)
	(concat msg ": ")))))

(eval-when-compile (require 'cl-lib))
(require 'facemenu)

(defvar Man-switches)
(defvar manual-program)
(defvar imenu-max-items)
(defvar msb-menu-cond)
(defvar gud-perldb-history)
(defvar vc-rcs-header)
(defvar vc-sccs-header)

(defun cperl-choose-color (&rest list)
  "Old-fashioned way to set colors for syntax highlighting.
Affects faces specific to `cperl-mode` only.
Optional argument LIST defines the attribute list for the face."
  (let (answer)
    (while list
      (or answer
          (if (or (color-defined-p (car list))
		  (null (cdr list)))
	      (setq answer (car list))))
      (setq list (cdr list)))
    answer))

(defgroup cperl nil
  "Major mode for editing Perl code."
  :prefix "cperl-"
  :group 'languages
  :version "20.3")

(defgroup cperl-indentation-details nil
  "Indentation.
The option `cperl-file-style' (which see) can be used to set
several indentation options in one go, following popular
indentation styles."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-affected-by-hairy nil
  "Variables affected by `cperl-hairy'."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-autoinsert-details nil
  "Auto-insert tuneup."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-faces nil
  "Fontification colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-speed nil
  "Speed vs. validity tuneup."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-help-system nil
  "Help system tuneup."
  :prefix "cperl-"
  :group 'cperl)


(defcustom cperl-extra-newline-before-brace nil
  "Non-nil means that code blocks start on a new line.
This affects if, elsif, while, until, else, for, foreach and do
constructs look like:

	if ()
	{
	}

instead of:

	if () {
	}"
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-extra-newline-before-brace-multiline
  cperl-extra-newline-before-brace
  "Non-nil means the same as `cperl-extra-newline-before-brace'.
It is effective for constructs with multiline
if/unless/while/until/for/foreach condition."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-file-style nil
  "Indentation style to use in `cperl-mode'.
Setting this option will override options as given in
`cperl-style-alist' for the keyword provided here.  If nil, then
the individual options as customized are used.
\"PBP\" is the style recommended in the Book \"Perl Best
Practices\" by Damian Conway.  \"CPerl\" is the traditional style
of `cperl-mode', and \"PerlStyle\" follows the Perl documentation
in perlstyle.  The other styles have been developed for other
programming languages, mostly C."
  :type '(choice (const "PBP")
                 (const "CPerl")
                 (const "PerlStyle")
                 (const "GNU")
                 (const "C++")
                 (const "K&R")
                 (const "BSD")
                 (const "Whitesmith")
                 (const :tag "Default" nil))
  :group 'cperl-indentation-details
  :version "29.1")
;;;###autoload(put 'cperl-file-style 'safe-local-variable 'stringp)

(defcustom cperl-indent-level 2
  "Indentation of CPerl statements with respect to containing block."
  :type 'integer
  :group 'cperl-indentation-details)

;; It is not unusual to put both things like perl-indent-level and
;; cperl-indent-level in the local variable section of a file.  If only
;; one of perl-mode and cperl-mode is in use, a warning will be issued
;; about the variable.  Autoload these here, so that no warning is
;; issued when using either perl-mode or cperl-mode.
;;;###autoload(put 'cperl-indent-level 'safe-local-variable 'integerp)
;;;###autoload(put 'cperl-brace-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'cperl-continued-brace-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'cperl-label-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'cperl-continued-statement-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'cperl-extra-newline-before-brace 'safe-local-variable 'booleanp)
;;;###autoload(put 'cperl-merge-trailing-else 'safe-local-variable 'booleanp)

(defcustom cperl-lineup-step nil
  "`cperl-lineup' will always lineup at multiple of this number.
If nil, the value of `cperl-indent-level' will be used."
  :type '(choice (const nil) integer)
  :group 'cperl-indentation-details)

(defcustom cperl-brace-imaginary-offset 0
  "Imagined indentation of a Perl open brace that actually follows a statement.
An open brace following other text is treated as if it were this far
to the right of the start of its line."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-brace-offset 0
  "Extra indentation for braces, compared with other text in same context."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-label-offset -2
  "Offset of CPerl label lines relative to usual indentation."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-min-label-indent 1
  "Minimal offset of CPerl label lines."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-continued-statement-offset 2
  "Extra indent for lines not starting new statements."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-continued-brace-offset 0
  "Extra indent for substatements that start with open-braces.
This is in addition to `cperl-continued-statement-offset'."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-close-paren-offset -1
  "Extra indent for substatements that start with close-parenthesis."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-indent-wrt-brace t
  "Non-nil means indent statements in if/etc block relative brace, not if/etc.
Versions 5.2 ... 5.20 behaved as if this were nil."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-indent-subs-specially t
  "If non-nil, indent subs inside other blocks relative to \"sub\" keyword.
Otherwise, indent them relative to statement that contains the declaration.
This applies to, for example, hash values."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-auto-newline nil
  "Non-nil means automatically insert a newline between phrases.
This happens before and after braces and after colons and semicolons,
inserted in CPerl code.  The following \\[cperl-electric-backspace] will
remove the inserted whitespace.  Insertion after colons requires both
this variable and `cperl-auto-newline-after-colon' set."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-autoindent-on-semi nil
  "Non-nil means automatically indent after insertion of (semi)colon.
Active if `cperl-auto-newline' is false."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-auto-newline-after-colon nil
  "Non-nil means automatically newline even after colons.
Subject to `cperl-auto-newline' setting."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-tab-always-indent t
  "Non-nil means TAB in CPerl mode should always reindent the current line.
This does not depend on where in the line point is when the TAB command
is used."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-font-lock nil
  "Non-nil (and non-null) means CPerl buffers will use `font-lock-mode'.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-lbrace-space nil
  "Non-nil (and non-null) means { after $ should be preceded by ` '.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens-string "({[]})<"
  "String of parentheses that should be electric in CPerl.
Closing ones are electric only if the region is highlighted."
  :type 'string
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens nil
  "Non-nil (and non-null) means parentheses should be electric in CPerl.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens-mark (not (not window-system))
  "Not-nil means that electric parens look for active mark.
Default is yes if there is visual feedback on mark."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-electric-linefeed nil
  "If true, LFD should be hairy in CPerl.
Otherwise, \\<cperl-mode-map>\\[newline-and-indent] is hairy.
In any case these two mean plain and hairy linefeeds together.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-keywords nil
  "Not-nil (and non-null) means keywords are electric in CPerl.
Can be overwritten by `cperl-hairy' if nil.

Uses `abbrev-mode' to do the expansion.  If you want to use your
own abbrevs in `cperl-mode', but do not want keywords to be
electric, you must redefine `cperl-mode-abbrev-table': do
\\[edit-abbrevs], search for `cperl-mode-abbrev-table', and, in
that paragraph, delete the words that appear at the ends of lines and
that begin with \"cperl-electric\"."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-backspace-untabify t
  "Not-nil means electric-backspace will untabify in CPerl."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-hairy nil
  "Not-nil means most of the bells and whistles are enabled in CPerl.
Affects: `cperl-font-lock', `cperl-electric-lbrace-space',
`cperl-electric-parens', `cperl-electric-linefeed', `cperl-electric-keywords',
`cperl-info-on-command-no-prompt', `cperl-clobber-lisp-bindings',
`cperl-lazy-help-time'."
  :type 'boolean
  :group 'cperl-affected-by-hairy)

(defcustom cperl-comment-column 32
  "Column to put comments in CPerl (use \\[cperl-indent] to lineup with code)."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-indent-comment-at-column-0 nil
  "Non-nil means that comment started at column 0 should be indentable."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-vc-sccs-header '("($sccs) = ('%W\ %' =~ /(\\d+(\\.\\d+)+)/) ;")
  "Special version of `vc-sccs-header' that is used in CPerl mode buffers."
  :type '(repeat string)
  :group 'cperl)

(defcustom cperl-vc-rcs-header '("($rcs) = (' $Id\ $ ' =~ /(\\d+(\\.\\d+)+)/);")
  "Special version of `vc-rcs-header' that is used in CPerl mode buffers."
  :type '(repeat string)
  :group 'cperl)

(defcustom cperl-info-on-command-no-prompt nil
  "Not-nil (and non-null) means not to prompt on \\[cperl-info-on-command].
The opposite behavior is always available if prefixed with Control-c.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-clobber-lisp-bindings nil
  "Not-nil (and non-null) means not overwrite \\[describe-function].
The function is available on \\[cperl-info-on-command], \\[cperl-get-help].
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-lazy-help-time nil
  "Not-nil (and non-null) means to show lazy help after given idle time.
Can be overwritten by `cperl-hairy' to be 5 sec if nil."
  :type '(choice (const null) (const nil) integer)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-pod-face 'font-lock-comment-face
  "Face for POD highlighting."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-pod-head-face 'font-lock-variable-name-face
  "Face for POD highlighting.
Font for POD headers."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-here-face 'font-lock-string-face
  "Face for here-docs highlighting."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-invalid-face 'underline
  "Face for highlighting trailing whitespace."
  :type 'face
  :version "21.1"
  :group 'cperl-faces)
(make-obsolete-variable 'cperl-invalid-face
                        'show-trailing-whitespace "30.1")

(defcustom cperl-pod-here-fontify t
  "Not-nil after evaluation means to highlight POD and here-docs sections."
  :type 'boolean
  :group 'cperl-faces)

(defcustom cperl-fontify-m-as-s t
  "Not-nil means highlight 1arg regular expressions operators same as 2arg."
  :type 'boolean
  :group 'cperl-faces)

(defcustom cperl-highlight-variables-indiscriminately nil
  "Non-nil means perform additional highlighting on variables.
Currently only changes how scalar variables are highlighted.
Note that the variable is only read at initialization time for
the variable `cperl-font-lock-keywords-2', so changing it after you've
entered CPerl mode the first time will have no effect."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-pod-here-scan t
  "Not-nil means look for POD and here-docs sections during startup.
You can always make lookup from menu or using \\[cperl-find-pods-heres]."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-regexp-scan t
  "Not-nil means make marking of regular expression more thorough.
Effective only with `cperl-pod-here-scan'."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-hook-after-change t
  "Not-nil means install hook to know which regions of buffer are changed.
May significantly speed up delayed fontification.  Changes take effect
after reload."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-max-help-size 66
  "Non-nil means shrink-wrapping of info-buffer allowed up to these percents."
  :type '(choice integer (const nil))
  :group 'cperl-help-system)

(defcustom cperl-shrink-wrap-info-frame t
  "Non-nil means shrink-wrapping of info-buffer-frame allowed."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-info-page "perl"
  "Name of the Info manual containing perl docs.
Older version of this page was called `perl5', newer `perl'."
  :type 'string
  :group 'cperl-help-system)
(make-obsolete-variable 'cperl-info-page
                        (concat "The Perl info page is no longer maintained. "
                                "Consider installing the perl-doc package from "
                                "GNU ELPA to access Perl documentation.")
                        "30.1")

(defcustom cperl-use-syntax-table-text-property t
  "Non-nil means CPerl sets up and uses `syntax-table' text property."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-use-syntax-table-text-property-for-tags
  cperl-use-syntax-table-text-property
  "Non-nil means: set up and use `syntax-table' text property generating TAGS."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-scan-files-regexp "\\.\\([pP][Llm]\\|xs\\)$"
  "Regexp to match files to scan when generating TAGS."
  :type 'regexp
  :group 'cperl)

(defcustom cperl-noscan-files-regexp
  "/\\(\\.\\.?\\|SCCS\\|RCS\\|CVS\\|blib\\)$"
  "Regexp to match files/dirs to skip when generating TAGS."
  :type 'regexp
  :group 'cperl)

(defcustom cperl-regexp-indent-step nil
  "Indentation used when beautifying regexps.
If nil, the value of `cperl-indent-level' will be used."
  :type '(choice integer (const nil))
  :group 'cperl-indentation-details)

(defcustom cperl-indent-left-aligned-comments t
  "Non-nil means that the comment starting in leftmost column should indent."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-under-as-char nil
  "Non-nil means that the _ (underline) should be treated as word char."
  :type 'boolean
  :group 'cperl)
(make-obsolete-variable 'cperl-under-as-char 'superword-mode "24.4")

(defcustom cperl-message-electric-keyword t
  "Non-nil means that the `cperl-electric-keyword' prints a help message."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-indent-region-fix-constructs 1
  "Amount of space to insert between `}' and `else' or `elsif'.
Used by `cperl-indent-region'.  Set to nil to leave as is.
Values other than 1 and nil will probably not work."
  :type '(choice (const nil) (const 1))
  :group 'cperl-indentation-details)

(defcustom cperl-break-one-line-blocks-when-indent t
  "Non-nil means that one-line blocks are reformatted when indenting.
Code blocks after if/unless/while/until/for/foreach need to be
reformatted into multiline ones when indenting a region."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-fix-hanging-brace-when-indent t
  "Non-nil means that BLOCK-end `}' may be put on a separate line.
This happens when indenting a region.  Braces followed by
else/elsif/while/until are excepted."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-merge-trailing-else t
  "Controls indentation of block-end `}' followed by else/elsif/continue.
If non-nil, then these block-end braces may be merged to be on the same
line when indenting a region."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-indent-parens-as-block nil
  "Non-nil means that non-block ()-, {}- and []-groups are indented as blocks.
However, trailing \",\" inside the group, won't increase indentation.
One should tune up `cperl-close-paren-offset' as well."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-syntaxify-by-font-lock t
  "Non-nil means that CPerl uses the `font-lock' routines for syntaxification."
  :type '(choice (const message) boolean)
  :group 'cperl-speed)

(defcustom cperl-syntaxify-unwind
  t
  "Controls the accuracy of syntaxification.
Non-nil means that CPerl unwinds to a start of a long construction
when syntaxifying a chunk of buffer."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-syntaxify-for-menu
  t
  "Non-nil means that CPerl syntaxifies up to the point before showing menu.
This way enabling/disabling of menu items is more correct."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-fontify-trailer
  'perl-code
  "How to treat text after an \"__END__\" or \"__DATA__\" token.
If \"perl-code\", treat as Perl code for fontification, examine for
imenu entries, and indent according to Perl syntax.  Use this setting if
you have trailing POD documentation, or for modules which use AutoLoad
or AutoSplit.  If \"comment\", treat as comment, do not look for imenu
entries, and do not change indentation."
  :type '(choice (const perl-code)
		 (const comment))
  :version "30.1"
  :group 'cperl-faces)

(defcustom cperl-ps-print-face-properties
  '((font-lock-keyword-face		nil nil		bold shadow)
    (font-lock-variable-name-face	nil nil		bold)
    (font-lock-function-name-face	nil nil		bold italic box)
    (font-lock-constant-face		nil "LightGray"	bold)
    (cperl-array-face			nil "LightGray"	bold underline)
    (cperl-hash-face			nil "LightGray"	bold italic underline)
    (font-lock-comment-face		nil "LightGray"	italic)
    (font-lock-string-face		nil nil		italic underline)
    (cperl-nonoverridable-face		nil nil		italic underline)
    (font-lock-type-face		nil nil		underline)
    (font-lock-warning-face		nil "LightGray"	bold italic box)
    (underline				nil "LightGray"	strikeout))
  "List given as an argument to `ps-extend-face-list' in `cperl-ps-print'."
  :type '(repeat (cons symbol
		       (cons (choice (const nil) string)
			     (cons (choice (const nil) string)
				   (repeat symbol)))))
  :group 'cperl-faces)

(defvar cperl-dark-background
  (cperl-choose-color "navy" "os2blue" "darkgreen"))
(defvar cperl-dark-foreground
  (cperl-choose-color "orchid1" "orange"))

(defface cperl-nonoverridable-face
  `((((class grayscale) (background light))
     (:background "Gray90" :slant italic :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :slant italic :underline t :weight bold))
    (((class color) (background light))
     (:foreground "chartreuse3"))
    (((class color) (background dark))
     (:foreground ,cperl-dark-foreground))
    (t (:weight bold :underline t)))
  "Font Lock mode face used non-overridable keywords and modifiers of regexps."
  :group 'cperl-faces)

(defface cperl-array-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))
    (((class color) (background light))
     (:foreground "Blue" :background "lightyellow2" :weight bold))
    (((class color) (background dark))
     (:foreground "yellow" :background ,cperl-dark-background :weight bold))
    (t (:weight bold)))
  "Font Lock mode face used to highlight array names."
  :group 'cperl-faces)

(defface cperl-hash-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold :slant italic))
    (((class color) (background light))
     (:foreground "Red" :background "lightyellow2" :weight bold :slant italic))
    (((class color) (background dark))
     (:foreground "Red" :background ,cperl-dark-background :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Font Lock mode face used to highlight hash names."
  :group 'cperl-faces)



;;; Short extra-docs.

(defvar cperl-tips 'please-ignore-this-line
  "Some tips for using `cperl-mode'.

If your Emacs does not default to `cperl-mode' on Perl files, and you
want it to: put the following into your .emacs file:

  (add-to-list \\='major-mode-remap-alist \\='(perl-mode . cperl-mode))

To read Perl documentation in info format you can convert POD to
texinfo with the converter `pod2texi' from the texinfo project:
  https://www.gnu.org/software/texinfo/manual/pod2texi.html

If some class structure is too complicated, use Tools/Hierarchy-view
from Perl menu, or hierarchic view of imenu.  The second one uses the
current buffer only, the first one requires generation of TAGS from
Perl/Tools/Tags menu beforehand.

Run Perl/Tools/Insert-spaces-if-needed to fix your lazy typing.

Switch auto-help on/off with Perl/Tools/Auto-help.

Though CPerl mode should maintain the correct parsing of Perl even when
editing, sometimes it may be lost.  Fix this by

  \\[normal-mode]

In cases of more severe confusion sometimes it is helpful to do

  \\[load-library] `cperl-mode' RET
  \\[normal-mode]

Before reporting (non-)problems look in the problem section of online
micro-docs on what I know about CPerl problems.")

(defvar cperl-problems 'please-ignore-this-line
  "Description of problems in CPerl mode.
`fill-paragraph' on a comment may leave the point behind the
paragraph.  It also triggers a bug in some versions of Emacs (CPerl tries
to detect it and bulk out).")

(defvar cperl-problems-old-emaxen 'please-ignore-this-line
  "Obsolete hints for outdated Emacs versions.
This used to contain a description of problems in CPerl mode
specific for very old Emacs versions.  This is no longer relevant
and has been removed.")
(make-obsolete-variable 'cperl-problems-old-emaxen nil "28.1")

(defvar cperl-praise 'please-ignore-this-line
  "Advantages of CPerl mode.

0) It uses the newest `syntax-table' property ;-);

1) It does 99% of Perl syntax correct.

When using `syntax-table' property for syntax assist hints, it should
handle 99.995% of lines correct - or somesuch.  It automatically
updates syntax assist hints when you edit your script.

2) It is generally believed to be \"the most user-friendly Emacs
package\" whatever it may mean (I doubt that the people who say similar
things tried _all_ the rest of Emacs ;-), but this was not a lonely
voice);

3) Everything is customizable, one-by-one or in a big sweep;

4) It has many easily-accessible \"tools\":
        a) Can run program, check syntax, start debugger;
        b) Can lineup vertically \"middles\" of rows, like `=' in
                a  = b;
                cc = d;
        c) Can insert spaces where this improves readability (in one
                interactive sweep over the buffer);
        d) Has support for imenu, including:
                1) Separate unordered list of \"interesting places\";
                2) Separate TOC of POD sections;
                3) Separate list of packages/classes;
                4) Hierarchical view of methods in (sub)packages;
                5) and functions (by the full name - with package);
        e) Has a builtin list of one-line explanations for perl constructs.
        f) Can show these explanations if you stay long enough at the
                corresponding place (or on demand);
        g) Has an enhanced fontification (using 3 or 4 additional faces
                comparing to font-lock - basically, different
                namespaces in Perl have different colors);
        h) Can construct TAGS basing on its knowledge of Perl syntax,
                the standard menu has 6 different way to generate
                TAGS (if \"by directory\", .xs files - with C-language
                bindings - are included in the scan);
        i) Can build a hierarchical view of classes (via imenu) basing
                on generated TAGS file;
        j) Has electric parentheses, electric newlines, uses Abbrev
                for electric logical constructs
                        while () {}
                with different styles of expansion (context sensitive
                to be not so bothering).  Electric parentheses behave
                \"as they should\" in a presence of a visible region.
        k) Changes msb.el \"on the fly\" to insert a group \"Perl files\";
        l) Can convert from
		if (A) { B }
	   to
		B if A;

        m) Highlights (by user-choice) either 3-delimiters constructs
	   (such as tr/a/b/), or regular expressions and `y/tr';
        o) Is able to manipulate Perl Regular Expressions to ease
	   conversion to a more readable form.
        p) Can ispell POD sections and HERE-DOCs.
        q) Understands comments and character classes inside regular
	   expressions; can find matching () and [] in a regular expression.
        r) Allows indentation of //x-style regular expressions;
        s) Highlights different symbols in regular expressions according
	   to their function; much less problems with backslashitis;
        t) Allows you to locate regular expressions which contain
	   interpolated parts.

5) The indentation engine was very smart, but most of tricks may be
not needed anymore with the support for `syntax-table' property.  Has
progress indicator for indentation (with `imenu' loaded).

6) Indent-region improves inline-comments as well; also corrects
whitespace *inside* the conditional/loop constructs.

7) Fill-paragraph correctly handles multi-line comments;

8) Can switch to different indentation styles by one command, and restore
the settings present before the switch.

9) When doing indentation of control constructs, may correct
line-breaks/spacing between elements of the construct.

10) Uses a linear-time algorithm for indentation of regions.

11) Syntax-highlight, indentation, sexp-recognition inside regular expressions.")

(defvar cperl-speed 'please-ignore-this-line
  "Considerations about performance of `cperl-mode'.

This is an incomplete compendium of what is available in other parts
of CPerl documentation.  Nowadays the performance of `cperl-mode'
is not as relevant as it used to be when this was written.
\(Please inform me if I skipped anything.)

There is a perception that CPerl is slower than alternatives.  This part
of documentation is designed to overcome this misconception.

*By default* CPerl tries to enable the most comfortable settings.
From most points of view, correctly working package is infinitely more
comfortable than a non-correctly working one, thus by default CPerl
prefers correctness over speed.  Below is the guide how to change
settings if your preferences are different.

A)  Speed of loading the file.  When loading file, CPerl may perform a
scan which indicates places which cannot be parsed by primitive Emacs
syntax-parsing routines, and marks them up so that either

    A1) CPerl may work around these deficiencies (for big chunks, mostly
        PODs and HERE-documents), or
    A2) CPerl will use improved syntax-handling which reads mark-up
        hints directly.

    The scan in case A2 is much more comprehensive, thus may be slower.

    User can disable syntax-engine-helping scan of A2 by setting
       `cperl-use-syntax-table-text-property'
    variable to nil (if it is set to t).

    One can disable the scan altogether (both A1 and A2) by setting
       `cperl-pod-here-scan'
    to nil.

B) Speed of editing operations.

    One can add a (minor) speedup to editing operations by setting
       `cperl-use-syntax-table-text-property'
    variable to nil (if it is set to t).  This will disable
    syntax-engine-helping scan, thus will make many more Perl
    constructs be wrongly recognized by CPerl, thus may lead to
    wrongly matched parentheses, wrong indentation, etc.

    One can unset `cperl-syntaxify-unwind'.  This might speed up editing
    of, say, long POD sections.")

(defvar cperl-tips-faces 'please-ignore-this-line
  "CPerl mode uses the following faces for highlighting:

  `cperl-array-face'			Array names
  `cperl-hash-face'			Hash names
  `font-lock-comment-face'	Comments, PODs and whatever is considered
				syntactically to be not code
  `font-lock-constant-face'	HERE-doc delimiters, labels, delimiters of
				2-arg operators s/y/tr/ or of RExen,
  `font-lock-warning-face'	Special-cased m// and s//foo/,
  `font-lock-function-name-face' _ as a target of a file tests, file tests,
				subroutine names at the moment of definition
				(except those conflicting with Perl operators),
				package names (when recognized), format names
  `font-lock-keyword-face'	Control flow switch constructs, declarators
  `cperl-nonoverridable-face'	Non-overridable keywords, modifiers of RExen
  `font-lock-string-face'	Strings, qw() constructs, RExen, POD sections,
				literal parts and the terminator of formats
				and whatever is syntactically considered
				as string literals
  `font-lock-type-face'		Overridable keywords
  `font-lock-variable-name-face' Variable declarations, indirect array and
				hash names, POD headers/item names

Note that in several situations the highlighting tries to inform about
possible confusion, such as different colors for function names in
declarations depending on what they (do not) override, or special cases
m// and s/// which do not do what one would expect them to do.

Help with best setup of these faces for printout requested (for each of
the faces: please specify bold, italic, underline, shadow and box.)

In regular expressions (including character classes):
  `font-lock-string-face'	\"Normal\" stuff and non-0-length constructs
  `font-lock-constant-face':	Delimiters
  `font-lock-warning-face'	Special-cased m// and s//foo/,
				Mismatched closing delimiters, parens
				we couldn't match, misplaced quantifiers,
				unrecognized escape sequences
  `cperl-nonoverridable-face'	Modifiers, as gism in m/REx/gism
  `font-lock-type-face'		escape sequences with arguments
                                (\\x \\23 \\p \\N)
				and others match-a-char escape sequences
  `font-lock-keyword-face'	Capturing parens, and |
  `font-lock-function-name-face' Special symbols: $ ^ . [ ] [^ ] (?{ }) (??{ })
				\"Range -\" in character classes
  `font-lock-builtin-face'	\"Remaining\" 0-length constructs, multipliers
				?+*{}, not-capturing parens, leading
				backslashes of escape sequences
  `font-lock-variable-name-face' Interpolated constructs, embedded code,
				POSIX classes (inside charclasses)
  `font-lock-comment-face'	Embedded comments")



;;; Portability stuff:

(defvar cperl-del-back-ch
  (car (append (where-is-internal 'delete-backward-char)
	       (where-is-internal 'backward-delete-char-untabify)))
  "Character generated by key bound to `delete-backward-char'.")

(and (vectorp cperl-del-back-ch) (= (length cperl-del-back-ch) 1)
     (setq cperl-del-back-ch (aref cperl-del-back-ch 0)))

(defun cperl-putback-char (c)
  "Obsolete.  Put C back to the event loop."
  (declare (obsolete nil "29.1"))
  (push c unread-command-events))

(defsubst cperl-put-do-not-fontify (from to &optional post)
  "Pretend that text between FROM and TO is already fontified.
If POST, do not do it with postponed fontification"
  (if (and post cperl-syntaxify-by-font-lock)
      nil
    (put-text-property (max (point-min) (1- from))
                       to 'fontified t)))

(defcustom cperl-mode-hook nil
  "Hook run by CPerl mode."
  :type 'hook
  :group 'cperl)

(defvar cperl-syntax-state nil)
(defvar cperl-syntax-done-to nil)

;; Make customization possible "in reverse"
(defsubst cperl-val (symbol &optional default hairy)
  "Internal feature to distinguish SYMBOL between \"uncustomized\" and nil.
Apply DEFAULT if nil, use HAIRY if `cperl-hairy' is non-nil.  Use the
symbol's value otherwise."
  (cond
   ((eq (symbol-value symbol) 'null) default)
   (cperl-hairy (or hairy t))
   (t (symbol-value symbol))))


(defun cperl-make-indent (column &optional minimum keep)
  "Indent from point with tabs and spaces until COLUMN is reached.
MINIMUM is like in `indent-to', which see.
Unless KEEP, removes the old indentation."
  (or keep
      (delete-horizontal-space))
  (indent-to column minimum))

(define-abbrev-table 'cperl-mode-electric-keywords-abbrev-table
  (mapcar (lambda (x)
            (let ((name (car x))
                  (fun (cadr x)))
              (list name name fun :system t)))
          '(("if" cperl-electric-keyword)
            ("elsif" cperl-electric-keyword)
            ("while" cperl-electric-keyword)
            ("until" cperl-electric-keyword)
            ("unless" cperl-electric-keyword)
            ("else" cperl-electric-else)
            ("continue" cperl-electric-else)
            ("for" cperl-electric-keyword)
            ("foreach" cperl-electric-keyword)
            ("formy" cperl-electric-keyword)
            ("foreachmy" cperl-electric-keyword)
            ("do" cperl-electric-keyword)
            ("=pod" cperl-electric-pod)
            ("=begin" cperl-electric-pod t)
            ("=over" cperl-electric-pod)
            ("=head1" cperl-electric-pod)
            ("=head2" cperl-electric-pod)
            ("pod" cperl-electric-pod)
            ("over" cperl-electric-pod)
            ("head1" cperl-electric-pod)
            ("head2" cperl-electric-pod)))
  "Abbrev table for electric keywords.  Controlled by `cperl-electric-keywords'."
  :case-fixed t
  :enable-function (lambda () (cperl-val 'cperl-electric-keywords)))

(define-abbrev-table 'cperl-mode-abbrev-table ()
  "Abbrev table in use in CPerl mode buffers."
  :parents (list cperl-mode-electric-keywords-abbrev-table))

(defvar cperl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{" 'cperl-electric-lbrace)
    (define-key map "[" 'cperl-electric-paren)
    (define-key map "(" 'cperl-electric-paren)
    (define-key map "<" 'cperl-electric-paren)
    (define-key map "}" 'cperl-electric-brace)
    (define-key map "]" 'cperl-electric-rparen)
    (define-key map ")" 'cperl-electric-rparen)
    (define-key map ";" 'cperl-electric-semi)
    (define-key map ":" 'cperl-electric-terminator)
    (define-key map "\C-c\C-j" 'cperl-linefeed)
    (define-key map "\C-c\C-t" 'cperl-invert-if-unless)
    (define-key map "\C-c\C-a" 'cperl-toggle-auto-newline)
    (define-key map "\C-c\C-k" 'cperl-toggle-abbrev)
    (define-key map "\C-c\C-w" 'cperl-toggle-construct-fix)
    (define-key map "\C-c\C-f" 'auto-fill-mode)
    (define-key map "\C-c\C-e" 'cperl-toggle-electric)
    (define-key map "\C-c\C-b" 'cperl-find-bad-style)
    (define-key map "\C-c\C-p" 'cperl-pod-spell)
    (define-key map "\C-c\C-d" 'cperl-here-doc-spell)
    (define-key map "\C-c\C-n" 'cperl-narrow-to-here-doc)
    (define-key map "\C-c\C-v" 'cperl-next-interpolated-REx)
    (define-key map "\C-c\C-x" 'cperl-next-interpolated-REx-0)
    (define-key map "\C-c\C-y" 'cperl-next-interpolated-REx-1)
    (define-key map "\C-c\C-ha" 'cperl-toggle-autohelp)
    (define-key map "\C-c\C-hp" 'cperl-perldoc)
    (define-key map "\C-c\C-hP" 'cperl-perldoc-at-point)
    (define-key map "\e\C-q" 'cperl-indent-exp) ; Usually not bound
    (define-key map [(control meta ?|)] 'cperl-lineup)
    ;;(define-key map "\M-q" 'cperl-fill-paragraph)
    ;;(define-key map "\e;" 'cperl-indent-for-comment)
    (define-key map "\177" 'cperl-electric-backspace)
    (define-key map "\t" 'cperl-indent-command)
    ;; don't clobber the backspace binding:
    (define-key map [(control ?c) (control ?h) ?F] 'cperl-perldoc)
    (if (cperl-val 'cperl-clobber-lisp-bindings)
        (progn
	  (define-key map [(control ?h) ?f]
	    ;;(concat (char-to-string help-char) "f") ; does not work
	    'cperl-perldoc)
	  (define-key map [(control ?h) ?v]
	    ;;(concat (char-to-string help-char) "v") ; does not work
	    'cperl-get-help)
	  (define-key map [(control ?c) (control ?h) ?f]
	    ;;(concat (char-to-string help-char) "f") ; does not work
	    (key-binding "\C-hf"))
	  (define-key map [(control ?c) (control ?h) ?v]
	    ;;(concat (char-to-string help-char) "v") ; does not work
	    (key-binding "\C-hv")))
      (define-key map [(control ?c) (control ?h) ?f]
        'cperl-perldoc)
      (define-key map [(control ?c) (control ?h) ?v]
	;;(concat (char-to-string help-char) "v") ; does not work
	'cperl-get-help))
    (define-key map [remap indent-sexp]        #'cperl-indent-exp)
    (define-key map [remap indent-region]      #'cperl-indent-region)
    (define-key map [remap indent-for-comment] #'cperl-indent-for-comment)
    map)
  "Keymap used in CPerl mode.")

(defvar cperl-lazy-installed)
(defvar cperl-old-style nil)
(easy-menu-define cperl-menu cperl-mode-map
  "Menu for CPerl mode."
  '("Perl"
    ["Beginning of function" beginning-of-defun t]
    ["End of function" end-of-defun t]
    ["Mark function" mark-defun t]
    ["Indent expression" cperl-indent-exp t]
    ["Fill paragraph/comment" fill-paragraph t]
    "----"
    ["Line up a construction" cperl-lineup (use-region-p)]
    ["Invert if/unless/while etc" cperl-invert-if-unless t]
    ("Regexp"
     ["Beautify" cperl-beautify-regexp
      cperl-use-syntax-table-text-property]
     ["Beautify one level deep" (cperl-beautify-regexp 1)
      cperl-use-syntax-table-text-property]
     ["Beautify a group" cperl-beautify-level
      cperl-use-syntax-table-text-property]
     ["Beautify a group one level deep" (cperl-beautify-level 1)
      cperl-use-syntax-table-text-property]
     ["Contract a group" cperl-contract-level
      cperl-use-syntax-table-text-property]
     ["Contract groups" cperl-contract-levels
      cperl-use-syntax-table-text-property]
     "----"
     ["Find next interpolated" cperl-next-interpolated-REx
      (next-single-property-change (point-min) 'REx-interpolated)]
     ["Find next interpolated (no //o)"
      cperl-next-interpolated-REx-0
      (or (text-property-any (point-min) (point-max) 'REx-interpolated t)
          (text-property-any (point-min) (point-max) 'REx-interpolated 1))]
     ["Find next interpolated (neither //o nor whole-REx)"
      cperl-next-interpolated-REx-1
      (text-property-any (point-min) (point-max) 'REx-interpolated t)])
    ["Insert spaces if needed to fix style" cperl-find-bad-style t]
    ["Refresh \"hard\" constructions" cperl-find-pods-heres t]
    "----"
    ["Indent region" cperl-indent-region (use-region-p)]
    ["Comment region" cperl-comment-region (use-region-p)]
    ["Uncomment region" cperl-uncomment-region (use-region-p)]
    "----"
    ["Debugger" cperl-db t]
    "----"
    ("Tools"
     ["Imenu" imenu]
     "----"
     ["Ispell PODs" cperl-pod-spell
      ;; Better not to update syntaxification here:
      ;; debugging syntaxification can be broken by this???
      (or
       (get-text-property (point-min) 'in-pod)
       (< (progn
            (and cperl-syntaxify-for-menu
                 (cperl-update-syntaxification (point-max)))
            (next-single-property-change (point-min) 'in-pod nil (point-max)))
          (point-max)))]
     ["Ispell HERE-DOCs" cperl-here-doc-spell
      (< (progn
           (and cperl-syntaxify-for-menu
                (cperl-update-syntaxification (point-max)))
           (next-single-property-change (point-min) 'here-doc-group nil (point-max)))
         (point-max))]
     ["Narrow to this HERE-DOC" cperl-narrow-to-here-doc
      (eq 'here-doc  (progn
                       (and cperl-syntaxify-for-menu
                            (cperl-update-syntaxification (point)))
                       (get-text-property (point) 'syntax-type)))]
     ["Select this HERE-DOC or POD section"
      cperl-select-this-pod-or-here-doc
      (memq (progn
              (and cperl-syntaxify-for-menu
                   (cperl-update-syntaxification (point)))
              (get-text-property (point) 'syntax-type))
            '(here-doc pod))]
     "----"
     ["CPerl pretty print (experimental)" cperl-ps-print]
     "----"
     ["Syntaxify region" cperl-find-pods-heres-region
      (use-region-p)]
     ["Profile syntaxification" cperl-time-fontification t]
     ["Debug errors in delayed fontification" cperl-emulate-lazy-lock t]
     ["Debug unwind for syntactic scan" cperl-toggle-set-debug-unwind t]
     ["Debug backtrace on syntactic scan (BEWARE!!!)"
      (cperl-toggle-set-debug-unwind nil t) t]
     "----"
     ["Class Hierarchy from TAGS" cperl-tags-hier-init t]
     ;;["Update classes" (cperl-tags-hier-init t) tags-table-list]
     ("Tags"
      ["Create tags for current file" (cperl-write-tags nil t) t]
      ["Add tags for current file" (cperl-write-tags) t]
      ["Create tags for Perl files in directory"
       (cperl-write-tags nil t nil t) t]
      ["Add tags for Perl files in directory"
       (cperl-write-tags nil nil nil t) t]
      ["Create tags for Perl files in (sub)directories"
       (cperl-write-tags nil t t t) t]
      ["Add tags for Perl files in (sub)directories"
       (cperl-write-tags nil nil t t) t]))
    ("Perl docs"
     ["Define word at point" imenu-go-find-at-position
      ;; This is from imenu-go.el.  I can't find it on any ELPA
      ;; archive, so I'm not sure if it's still in use or not.
      (fboundp 'imenu-go-find-at-position)]
     ["Help on symbol at point" cperl-get-help t]
     ["Perldoc" cperl-perldoc t]
     ["Perldoc on word at point" cperl-perldoc-at-point t]
     ["View manpage of POD in this file" cperl-build-manpage t]
     ["Auto-help on" cperl-lazy-install
      (not cperl-lazy-installed)]
     ["Auto-help off" cperl-lazy-unstall
      cperl-lazy-installed])
    ("Toggle..."
     ["Auto newline" cperl-toggle-auto-newline t]
     ["Electric parens" cperl-toggle-electric t]
     ["Electric keywords" cperl-toggle-abbrev t]
     ["Extra paired delimiters" cperl-extra-paired-delimiters-mode t]
     ["Fix whitespace on indent" cperl-toggle-construct-fix t]
     ["Auto-help on Perl constructs" cperl-toggle-autohelp t]
     ["Auto fill" auto-fill-mode t])
    ("Default indent styles..."
     ["CPerl" (cperl-set-style "CPerl") t]
     ["PBP" (cperl-set-style  "PBP") t]
     ["PerlStyle" (cperl-set-style "PerlStyle") t]
     ["GNU" (cperl-set-style "GNU") t]
     ["C++" (cperl-set-style "C++") t]
     ["K&R" (cperl-set-style "K&R") t]
     ["BSD" (cperl-set-style "BSD") t]
     ["Whitesmith" (cperl-set-style "Whitesmith") t]
     ["Memorize Current" (cperl-set-style "Current") t]
     ["Memorized" (cperl-set-style-back) cperl-old-style])
    ("Indent styles for current buffer..."
     ["CPerl" (cperl-set-style "CPerl") t]
     ["PBP" (cperl-file-style  "PBP") t]
     ["PerlStyle" (cperl-file-style "PerlStyle") t]
     ["GNU" (cperl-file-style "GNU") t]
     ["C++" (cperl-file-style "C++") t]
     ["K&R" (cperl-file-style "K&R") t]
     ["BSD" (cperl-file-style "BSD") t]
     ["Whitesmith" (cperl-file-style "Whitesmith") t])
    ("Micro-docs"
     ["Tips" (describe-variable 'cperl-tips) t]
     ["Problems" (describe-variable 'cperl-problems) t]
     ["Speed" (describe-variable 'cperl-speed) t]
     ["Praise" (describe-variable 'cperl-praise) t]
     ["Faces" (describe-variable 'cperl-tips-faces) t]
     ["CPerl mode" (describe-function 'cperl-mode) t])))

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)


;;; Perl Grammar Components
;;
;; The following regular expressions are building blocks for a
;; minimalistic Perl grammar, to be used instead of individual (and
;; not always consistent) literal regular expressions.

;; This is necessary to compile this file under Emacs 26.1
;; (there's no rx-define which would help)
(eval-and-compile

  (defconst cperl--basic-identifier-rx
    ;; The rx expression in the following line is a workaround for
    ;; bug#70948 under Emacs 29
    '(regex "[_[:alpha:]][_[:word:]]*")
    ;; The rx expression in the following line is equivalent but
    ;; inefficient under Emacs 29.3
    ;; '(sequence (or alpha "_") (* (or word "_")))
    "A regular expression for the name of a \"basic\" Perl variable.
Neither namespace separators nor sigils are included.  As is,
this regular expression applies to labels,subroutine calls where
the ampersand sigil is not required, and names of attributes.")

  (defconst cperl--label-rx
    `(sequence symbol-start
               ,cperl--basic-identifier-rx
               (0+ space)
               ":")
    "A regular expression for a Perl label.
By convention, labels are uppercase alphabetics, but this isn't
enforced.")

  (defconst cperl--false-label-rx
    '(sequence (or (in "sym") "tr") (0+ space) ":")
    "A regular expression which is similar to a label, but might as
well be a quote-like operator with a colon as delimiter.")

  (defconst cperl--normal-identifier-rx
    `(or (sequence (1+ (sequence
                        (opt ,cperl--basic-identifier-rx)
                        "::"))
                   (opt ,cperl--basic-identifier-rx))
         ,cperl--basic-identifier-rx)
    "A regular expression for a Perl variable name with optional namespace.
Examples are `foo`, `Some::Module::VERSION`, and `::` (yes, that
is a legal variable name).")

  (defconst cperl--special-identifier-rx
    '(or
      (1+ digit)                          ; $0, $1, $2, ...
      (sequence "^" (any "A-Z" "]^_?\\")) ; $^V
      (sequence "{" (0+ space)            ; ${^MATCH}
                "^" (any "A-Z" "]^_?\\")
                (0+ (any "A-Z" "_" digit))
                (0+ space) "}")
      (in "!\"$%&'()+,-./:;<=>?@\\]^_`|~"))   ; $., $|, $", ... but not $^ or ${
    "The list of Perl \"punctuation\" variables, as listed in perlvar.")

  (defconst cperl--basic-scalar-rx
    `(sequence "$" ,cperl--basic-identifier-rx)
    "Regular expression for a scalar (without package).
This regexp intentionally does not support spaces (nor newlines
and comments) between the sigil and the identifier, for
educational reasons.  So \"$foo\" will be matched, but \"$ foo\"
or \"${ foo }\" will not.")

  (defconst cperl--basic-array-rx
    `(sequence "@" ,cperl--basic-identifier-rx)
    "Regular expression for an array variable (without package).
This regexp intentionally does not support spaces (nor newlines
and comments) between the sigil and the identifier, for
educational reasons.  So \"@foo\" will be matched, but \"@ foo\"
or \"@{ foo }\" will not.")

  (defconst cperl--basic-hash-rx
    `(sequence "%" ,cperl--basic-identifier-rx)
    "Regular expression for a hash variable (without package).
This regexp intentionally does not support spaces (nor newlines
and comments) between the sigil and the identifier, for
educational reasons.  So \"%foo\" will be matched, but \"% foo\"
or \"%{ foo }\" will not.")

  (defconst cperl--ws-rx
    '(sequence (or space "\n"))
    "Regular expression for a single whitespace in Perl.")

  (defconst cperl--eol-comment-rx
    '(sequence "#" (0+ (not (in "\n"))) "\n")
    "Regular expression for a single end-of-line comment in Perl")

  (defconst cperl--ws-or-comment-rx
    `(or ,cperl--ws-rx
         ,cperl--eol-comment-rx)
    "A regular expression for either whitespace or comment")

  (defconst cperl--ws*-rx
    `(0+ ,cperl--ws-or-comment-rx)
    "Regular expression for optional whitespaces or comments in Perl")

  (defconst cperl--ws+-rx
    `(1+ ,cperl--ws-or-comment-rx)
    "Regular expression for a sequence of whitespace and comments in Perl.")

  (defconst cperl--basic-variable-rx
    `(sequence (in "$@%") ,cperl--basic-identifier-rx)
    "Regular expression for a Perl variable (scalar, array or hash).
This regexp intentionally does not support spaces (nor newlines
and comments) between the sigil and the identifier, for
educational reasons.  So \"$foo\" will be matched, but \"$ foo\"
or \"${ foo }\" will not.")

  (defconst cperl--variable-list-rx
    `(sequence "("
	       (optional (sequence
                          ,cperl--ws*-rx
                          ,cperl--basic-variable-rx
			  (0+ (sequence
			       ,cperl--ws*-rx
			       ","
			       ,cperl--ws*-rx
			       ,cperl--basic-variable-rx))
                          ,cperl--ws*-rx)))
    "Regular expression for a list of Perl variables for declarations.")

  ;; This is left as a string regexp.  There are many version schemes in
  ;; the wild, so people might want to fiddle with this variable.
  (defconst cperl--version-regexp
    (rx-to-string
     `(or
       (sequence (optional "v")
	         (>= 2 (sequence (1+ digit) "."))
	         (1+ digit)
	         (optional (sequence "_" (1+ word))))
       (sequence (1+ digit)
	         (optional (sequence "." (1+ digit)))
	         (optional (sequence "_" (1+ word))))))
    "A sequence for recommended version number schemes in Perl.")

  (defconst cperl--single-attribute-rx
    `(sequence ,cperl--basic-identifier-rx
               (optional (sequence "("
                                   (0+ (or (sequence "\\" not-newline)
                                           (not (any "()\\"))
                                           (sequence "("
		                                     (zero-or-more
		                                      (not
		                                       (any "()\\")))
		                                     ")")))
                                   ")")))
    "A regular expression for a single attribute, without leading colon.
It may have parameters in parens, one level of parens within the
parameter's value is supported.  This regexp does not have
capture groups.")

  (defconst cperl--attribute-list-rx
    `(sequence ":"
               (optional
                ,cperl--ws*-rx
                ,cperl--single-attribute-rx
                (0+ (sequence
                     (or (sequence ,cperl--ws*-rx
                                   ":"
                                   ,cperl--ws*-rx)
                         ,cperl--ws+-rx)
                     ,cperl--single-attribute-rx))
                (optional ":")))
    "A regular expression for an attribute list.
Attribute lists may only occur in certain declarations.  A colon
is required before the first attribute but optional between
subsequent attributes.  This regexp does not have capture groups.")

  (defconst cperl--prototype-rx
    `(sequence "("
               (0+ (any "$@%&*;\\[]"))
               ")")
    "A regular expression for a subroutine prototype.  Not as strict
as the actual prototype syntax, but good enough to distinguish
prototypes from signatures.")

  (defconst cperl--signature-rx
    `(sequence "("
               (optional
                (sequence
                 (0+ (sequence ,cperl--ws*-rx
                               (or ,cperl--basic-scalar-rx "$")
                               ,cperl--ws*-rx
                               ","))
                 ,cperl--ws*-rx
                 (or ,cperl--basic-scalar-rx
                     ,cperl--basic-array-rx
                     ,cperl--basic-hash-rx
                     "$" "%" "@")))
               (optional (sequence ,cperl--ws*-rx) "," )
               ,cperl--ws*-rx
               ")")
    "A rx sequence subroutine signature without initializers.
These are a bit more restricted than \"my\" declaration lists
because they allow only one slurpy variable, and only in the last
place.")

  (defconst cperl--sloppy-signature-rx
    `(sequence "("
               ,cperl--ws*-rx
               (or ,cperl--basic-scalar-rx
                   ,cperl--basic-array-rx
                   ,cperl--basic-hash-rx)
               ,cperl--ws*-rx
               (or "," "=" "||=" "//=" ")"))
    "A rx sequence for the begin of a signature with initializers.
Initializers can contain almost all Perl constructs and thus can
not be covered by regular expressions.  This sequence captures
enough to distinguish a signature from a prototype.")

  (defconst cperl--package-rx
    `(sequence (group (or "package" "class"))
               ,cperl--ws+-rx
               (group ,cperl--normal-identifier-rx)
               (optional (sequence ,cperl--ws+-rx
                                   (group (regexp ,cperl--version-regexp)))))
    "A regular expression for package|class NAME VERSION in Perl.
Contains three groups for the initial keyword \"package\" or
\"class\", for the package name and for the version.")

  (defconst cperl--package-for-imenu-rx
    `(sequence symbol-start
               (group-n 1 "package")
               ,cperl--ws+-rx
               (group-n 2 ,cperl--normal-identifier-rx)
               (optional (sequence ,cperl--ws+-rx
                                   (regexp ,cperl--version-regexp)))
               ,cperl--ws*-rx
               (group-n 3 (or ";" "{")))
    "A regular expression to collect package names for `imenu'.
Catches \"package NAME;\", \"package NAME VERSION;\", \"package
NAME BLOCK\" and \"package NAME VERSION BLOCK.\" Contains three
groups: One for the keyword \"package\", one for the package
name, and one for the discovery of a following BLOCK.")

  ;; This gets a regexp of its own because classes allow attributes
  ;; (e.g. ":isa(Parent)") while packages don't.  We skip over it, but
  ;; like for "package" we capture the following ";" or "{".
  (defconst cperl--class-for-imenu-rx
    `(sequence (or space line-start)
               (group-n 1 "class")
               ,cperl--ws+-rx
               (group-n 2 ,cperl--normal-identifier-rx)
               (optional (sequence ,cperl--ws+-rx
                                   (regexp ,cperl--version-regexp)))
               (optional (sequence ,cperl--ws*-rx
                                   ,cperl--attribute-list-rx))
               ,cperl--ws*-rx
               (group-n 3 (or ";" "{")))
    "A regular expression to collect package names for `imenu'.
Catches \"class NAME;\", \"class NAME VERSION;\", \"class NAME
BLOCK\" and \"class NAME VERSION BLOCK\" and allows for
attributes like \":isa(Parent)\".  Contains three groups: One for
the keyword \"package\", one for the package name, and one for
the discovery of a following BLOCK.")

  (defconst cperl--sub-name-for-imenu-rx
    `(sequence symbol-start
               (optional (sequence (group-n 3 (or "my" "state" "our"))
	                           ,cperl--ws+-rx))
               (group-n 1 (or "method" "sub"))
               ,cperl--ws+-rx
               (group-n 2 ,cperl--normal-identifier-rx))
    "A regular expression to detect a subroutine or method start.
Contains three groups: One to distinguish lexical from \"normal\"
subroutines, for the keyword \"sub\" or \"method\", and one for
the subroutine name.")

  (defconst cperl--sub-name-generated-rx
    `(sequence symbol-start
               (optional (group-n 3 unmatchable))
               ;; autogenerated methods are not lexicals, so enforce the
               ;; third capture group to be nil
               (group-n 1 "field")
               ,cperl--ws+-rx
               ,cperl--basic-variable-rx
               ,cperl--ws+-rx
               (group-n 2 ,cperl--attribute-list-rx))
    "A regular expression to capture autogenerated reader methods.
The name of the method is either the field name without its sigil, or
given in parentheses after the \":reader\" or \":writer\" keyword.  More
than one attribute can be present: The match will be parsed in an extra
step.")

(defconst cperl--block-declaration-rx
  `(sequence
    (or "class" "method" "package" "sub")
    (1+ ,cperl--ws-or-comment-rx)
    ,cperl--normal-identifier-rx)
  "A regular expression to find a declaration for a named block.
Used for indentation.  These declarations introduce a block which
does not need a semicolon to terminate the statement.")

(defconst cperl--field-declaration-rx
  `(sequence
    "field"
    (1+ ,cperl--ws-or-comment-rx)
    ,cperl--basic-variable-rx
    (optional (sequence ,cperl--ws+-rx ,cperl--attribute-list-rx))
    )
  "A regular expression to find a declaration for a field.
Fields can have attributes for fontification, and even for imenu because
for example \":reader\" implicitly declares a method.")

(defconst cperl--pod-heading-rx
  `(sequence line-start
             (group-n 1 "=head")
             (group-n 3 (in "1-4"))
             (1+ (in " \t"))
             (group-n 2 (1+ (not (in "\n")))))
  "A regular expression to detect a POD heading.
Contains two groups: One for the heading level, and one for the
heading text.")

(defconst cperl--imenu-entries-rx
  `(or ,cperl--package-for-imenu-rx
       ,cperl--class-for-imenu-rx
       ,cperl--sub-name-for-imenu-rx
       ,cperl--sub-name-generated-rx
       ,cperl--pod-heading-rx)
  "A regular expression to collect stuff that goes into the `imenu' index.
Covers packages and classes, subroutines and methods, and POD headings.")

;; end of eval-and-compiled stuff
)


(defun cperl-declaration-header-p (pos)
  "Return t if POS is in the header of a declaration.
Perl syntax can have various constructs between a
keyword (e.g. \"sub\") and its associated block of code, and
these can span several lines.  These blocks are identified and
marked with a text-property in `cperl-find-pods-heres'.  This
function tests that property."
  (equal (get-text-property pos 'syntax-type) 'sub-decl))

(defun cperl-block-declaration-p ()
  "Test whether the following ?\\{ opens a declaration block.
Returns the column where the declaring keyword is found, or nil
if this isn't a declaration block.  Declaration blocks are named
subroutines, packages and the like.  They start with a keyword
and a name, to be followed by various descriptive items which are
just skipped over for our purpose.  Declaration blocks end a
statement, so there's no semicolon."
  ;; A scan error means that none of the declarators has been found
  (condition-case nil
      (let ((is-block-declaration nil)
            (continue-searching t))
        (while (and continue-searching (not (bobp)))
          (forward-sexp -1)
          (cond
           ((looking-at (rx (eval cperl--block-declaration-rx)))
            (setq is-block-declaration (current-column)
                  continue-searching nil))
           ((cperl-declaration-header-p (point))
            (setq is-block-declaration (current-column)
                  continue-searching nil))
           ;; Another brace means this is no block declaration
           ((looking-at "{")
            (setq continue-searching nil))
           (t
            (cperl-backward-to-noncomment (point-min))
            ;; A semicolon or an opening brace prevent this block from
            ;; being a block declaration
            (when (or (eq (preceding-char) ?\;)
                      (eq (preceding-char) ?{))
              (setq continue-searching nil)))))
        is-block-declaration)
    (error nil)))


;; These two must be unwound, otherwise take exponential time
(defconst cperl-maybe-white-and-comment-rex
  (rx (group (eval cperl--ws*-rx)))
  ;; was: "[ \t\n]*\\(#[^\n]*\n[ \t\n]*\\)*"
"Regular expression to match optional whitespace with interspersed comments.
Should contain exactly one group.")

;; This one is tricky to unwind; still very inefficient...
(defconst cperl-white-and-comment-rex
  (rx (group (eval cperl--ws+-rx)))
  ;; was: "\\([ \t\n]\\|#[^\n]*\n\\)+"
"Regular expression to match whitespace with interspersed comments.
Should contain exactly one group.")


;; Is incorporated in `cperl-outline-regexp', `defun-prompt-regexp'.
;; Details of groups in this may be used in several functions; see comments
;; near mentioned above variable(s)...
;; sub($$):lvalue{}  sub:lvalue{} Both allowed...
(defsubst cperl-after-sub-regexp (named attr) ; 9 groups without attr...
  "Match the text after `sub' in a subroutine declaration.
If NAMED is nil, allows anonymous subroutines.  Matches up to the first \":\"
of attributes (if present), or end of the name or prototype (whatever is
the last).
If ATTR is non-nil, also capture the attributes."
  (concat				; Assume n groups before this...
   "\\("				; n+1=name-group
     cperl-white-and-comment-rex	; n+2=pre-name
     (rx-to-string `(group ,cperl--normal-identifier-rx))
   "\\)"				; END n+1=name-group
   (if named "" "?")
   "\\("				; n+4=proto-group
     cperl-maybe-white-and-comment-rex	; n+5=pre-proto
     "\\(([^()]*)\\)"			; n+6=prototype
   "\\)?"				; END n+4=proto-group
   "\\("				; n+7=attr-group
     cperl-maybe-white-and-comment-rex	; n+8=pre-attr
     "\\("				; n+9=start-attr
        ":"
	(if attr (concat
		  "\\("
		     cperl-maybe-white-and-comment-rex ; whitespace-comments
		     "\\(\\<\\sw\\|_\\)+"	; attr-name
		     ;; attr-arg (1 level of internal parens allowed!)
		     "\\((\\(\\\\.\\|[^\\()]\\|([^\\()]*)\\)*)\\)?"
		     "\\("		; optional : (XXX allows trailing???)
		        cperl-maybe-white-and-comment-rex ; whitespace-comments
		     ":\\)?"
		  "\\)+")
	  "[^:]")
     "\\)"
   "\\)?"				; END n+6=proto-group
   ))

;; Tired of editing this in 8 places every time I remember that there
;; is another method-defining keyword
(defvar cperl-sub-keywords
  '("sub" "method"))

(defvar cperl-sub-regexp (regexp-opt cperl-sub-keywords))

(defun cperl-char-ends-sub-keyword-p (char)
  "Return t if CHAR is the last character of a perl sub keyword."
  (cl-loop for keyword in cperl-sub-keywords
           when (eq char (aref keyword (1- (length keyword))))
           return t))

(defvar cperl-outline-regexp
  (rx (sequence line-start (0+ blank) (eval cperl--imenu-entries-rx)))
  "The regular expression used for `outline-minor-mode'.")

(defvar cperl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?/  "."  st)
    (modify-syntax-entry ?*  "."  st)
    (modify-syntax-entry ?+  "."  st)
    (modify-syntax-entry ?-  "."  st)
    (modify-syntax-entry ?=  "."  st)
    (modify-syntax-entry ?%  "."  st)
    (modify-syntax-entry ?<  "."  st)
    (modify-syntax-entry ?>  "."  st)
    (modify-syntax-entry ?&  "."  st)
    (modify-syntax-entry ?$  "\\" st)
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?#  "<"  st)
    (modify-syntax-entry ?'  "\"" st)
    (modify-syntax-entry ?`  "\"" st)
    (if cperl-under-as-char
        (modify-syntax-entry ?_ "w" st))
    (modify-syntax-entry ?:  "_"  st)
    (modify-syntax-entry ?|  "."  st)
    st)
  "Syntax table in use in CPerl mode buffers.")

(defvar cperl-string-syntax-table
  (let ((st (copy-syntax-table cperl-mode-syntax-table)))
    (modify-syntax-entry ?$  "." st)
    (modify-syntax-entry ?\{ "." st)
    (modify-syntax-entry ?\} "." st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?'  "." st)
    (modify-syntax-entry ?`  "." st)
    (modify-syntax-entry ?#  "." st) ; (?# comment )
    st)
  "Syntax table in use in CPerl mode string-like chunks.")

(defsubst cperl-1- (p)
  "Decreases a position from P, but does not go before `point-min'."
  (max (point-min) (1- p)))

(defsubst cperl-1+ (p)
  "Increases a position from P, but does not go beyond `point-max'."
  (min (point-max) (1+ p)))



(defvar cperl-faces-init nil)
;; Fix for msb.el
(defvar cperl-msb-fixed nil)
(defvar cperl-use-major-mode 'cperl-mode)
(defvar cperl-font-locking nil)

(defvar cperl-compilation-error-regexp-list
  ;; This look like a paranoiac regexp: could anybody find a better one? (which WORKS).
  '("^[^\n]* \\(file\\|at\\) \\([^ \t\n]+\\) [^\n]*line \\([0-9]+\\)[\\., \n]"
    2 3)
  "List that specifies how to match errors in Perl output.")

(defvar cperl-compilation-error-regexp-alist)
(make-obsolete-variable 'cperl-compilation-error-regexp-alist
                        'cperl-compilation-error-regexp-list "28.1")

(defvar compilation-error-regexp-alist)

;;;###autoload
(define-derived-mode cperl-mode prog-mode "CPerl"
  "Major mode for editing Perl code.
Expression and list commands understand all C brackets.
Tab indents for Perl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Various characters in Perl almost always come in pairs: {}, (), [],
sometimes <>.  When the user types the first, she gets the second as
well, with optional special formatting done on {}.  (Disabled by
default.)  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion.  The processing of < is special,
since most the time you mean \"less\".  CPerl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate.  You can set `cperl-electric-parens-string' to the string that
contains the parens from the above list you want to be electrical.
Electricity of parens is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:

   if, else, elsif, unless, while, until, continue, do,
   for, foreach, formy and foreachmy.

and POD directives (Disabled by default, see `cperl-electric-keywords'.)

The user types the keyword immediately followed by a space, which
causes the construct to be expanded, and the point is positioned where
she is most likely to want to be.  E.g., when the user types a space
following \"if\" the following appears in the buffer: if () { or if ()
} { } and the cursor is between the parentheses.  The user can then
type some boolean expression within the parens.  Having done that,
typing \\[cperl-linefeed] places you - appropriately indented - on a
new line between the braces (if you typed \\[cperl-linefeed] in a POD
directive line, then appropriate number of new lines is inserted).

If CPerl decides that you want to insert \"English\" style construct like

            bite if angry;

it will not do any expansion.  See also help on variable
`cperl-extra-newline-before-brace'.  (Note that one can switch the
help message on expansion by setting `cperl-message-electric-keyword'
to nil.)

\\[cperl-linefeed] is a convenience replacement for typing carriage
return.  It places you in the next line with proper indentation, or if
you type it inside the inline block of control construct, like

            foreach (@lines) {print; print}

and you are on a boundary of a statement inside braces, it will
transform the construct into a multiline and will place you into an
appropriately indented blank line.  If you need a usual
`newline-and-indent' behavior, it is on \\[newline-and-indent],
see documentation on `cperl-electric-linefeed'.

Use \\[cperl-invert-if-unless] to change a construction of the form

	    if (A) { B }

into

            B if A;

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on `font-lock-mode',
`cperl-electric-lbrace-space' to t switches on electric space between $
and {, `cperl-electric-parens-string' is the string that contains
parentheses that should be electric in CPerl (see also
`cperl-electric-parens-mark' and `cperl-electric-parens'), setting
`cperl-electric-keywords' enables electric expansion of control
structures in CPerl.  `cperl-electric-linefeed' governs which one of two
linefeed behavior is preferable.  You can enable all these options
simultaneously by setting `cperl-hairy' to t.  In this case you can
switch separate options off by setting them to `null'.  Note that one may
undo the extra whitespace inserted by semis and braces in
`auto-newline'-mode by consequent \\[cperl-electric-backspace].

Short one-liner-style help is available on \\[cperl-get-help],
and one can run perldoc or man via menu.

It is possible to show this help automatically after some idle time.
This is regulated by variable `cperl-lazy-help-time'.  Default with
`cperl-hairy' (if the value of `cperl-lazy-help-time' is nil) is 5
secs idle time .  It is also possible to switch this on/off from the
menu, or via \\[cperl-toggle-autohelp].

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of POD and
here-docs sections.  Results of scan are used for indentation too.

Variables controlling indentation style:
 `cperl-tab-always-indent'
    Non-nil means TAB in CPerl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `cperl-indent-left-aligned-comments'
    Non-nil means that the comment starting in leftmost column should indent.
 `cperl-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in Perl code.  The following
    \\[cperl-electric-backspace] will remove the inserted whitespace.
    Insertion after colons requires both this variable and
    `cperl-auto-newline-after-colon' set.
 `cperl-auto-newline-after-colon'
    Non-nil means automatically newline even after colons.
    Subject to `cperl-auto-newline' setting.
 `cperl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `cperl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if, or body of a while, or just a statement continuation.
 `cperl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `cperl-continued-statement-offset'.
 `cperl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `cperl-brace-imaginary-offset'
    An open brace following other text is treated as if it the line started
    this far to the right of the actual line indentation.
 `cperl-label-offset'
    Extra indentation for line that is a label.
 `cperl-min-label-indent'
    Minimal indentation for line that is a label.

Settings for classic indent-styles: K&R BSD=C++ GNU PBP PerlStyle=Whitesmith
  `cperl-indent-level'                5   4       2   4   4
  `cperl-brace-offset'                0   0       0   0   0
  `cperl-continued-brace-offset'     -5  -4       0   0   0
  `cperl-label-offset'               -5  -4      -2  -2  -4
  `cperl-continued-statement-offset'  5   4       2   4   4

CPerl knows several indentation styles, and may bulk set the
corresponding variables.  Use \\[cperl-set-style] to do this or
set the variable `cperl-file-style' user option.  Use
\\[cperl-set-style-back] to restore the memorized preexisting
values \(both available from menu).  See examples in
`cperl-style-examples'.

Part of the indentation style is how different parts of if/elsif/else
statements are broken into lines; in CPerl, this is reflected on how
templates for these constructs are created (controlled by
`cperl-extra-newline-before-brace'), and how reflow-logic should treat
\"continuation\" blocks of else/elsif/continue, controlled by the same
variable, and by `cperl-extra-newline-before-brace-multiline',
`cperl-merge-trailing-else', `cperl-indent-region-fix-constructs'.

If `cperl-indent-level' is 0, the statement after opening brace in
column 0 is indented on
`cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args.

DO NOT FORGET to read micro-docs (available from `Perl' menu)
or as help on variables `cperl-tips', `cperl-problems',
`cperl-praise', `cperl-speed'."
  (if (cperl-val 'cperl-electric-linefeed)
      (progn
	(local-set-key "\C-J" 'cperl-linefeed)
	(local-set-key "\C-C\C-J" 'newline-and-indent)))
  (if (and
       (cperl-val 'cperl-clobber-lisp-bindings)
       (cperl-val 'cperl-info-on-command-no-prompt))
      (progn
	;; don't clobber the backspace binding:
	(define-key cperl-mode-map "\C-hf" 'cperl-perldoc)
	(define-key cperl-mode-map "\C-c\C-hf" 'cperl-perldoc)))
  (setq local-abbrev-table cperl-mode-abbrev-table)
  (if (cperl-val 'cperl-electric-keywords)
      (abbrev-mode 1))
  (set-syntax-table cperl-mode-syntax-table)
  ;; Workaround for Bug#30393, needed for Emacs 26.
  (when (< emacs-major-version 27)
    (setq-local open-paren-in-column-0-is-defun-start nil))
  ;; Until Emacs is multi-threaded, we do not actually need it local:
  (make-local-variable 'cperl-font-locking)
  (setq-local outline-regexp cperl-outline-regexp)
  (setq-local outline-level 'cperl-outline-level)
  (setq-local add-log-current-defun-function
              (lambda ()
                (save-excursion
                  (if (re-search-backward "^sub[ \t]+\\([^({ \t\n]+\\)" nil t)
                      (match-string-no-properties 1)))))

  (setq-local paragraph-start (concat "^$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function #'cperl-indent-line)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-column cperl-comment-column)
  (setq-local comment-start-skip "#+ *")

;;       "[ \t]*sub"
;;	  (cperl-after-sub-regexp 'named nil) ; 8=name 11=proto 14=attr-start
;;	  cperl-maybe-white-and-comment-rex	; 15=pre-block
  (setq-local defun-prompt-regexp
              (concat "^[ \t]*\\("
                      cperl-sub-regexp
                      (cperl-after-sub-regexp 'named 'attr-groups)
                      (rx (eval cperl--ws*-rx))
                      (rx (optional (eval cperl--signature-rx)))
                      "\\|"			; per toke.c
                      (rx (or "ADJUST" "AUTOLOAD" "BEGIN" "CHECK" "DESTROY"
                              "END" "INIT" "UNITCHECK"))
                      "\\)"
                      cperl-maybe-white-and-comment-rex))
  (setq-local comment-indent-function #'cperl-comment-indent)
  (setq-local fill-paragraph-function #'cperl-fill-paragraph)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local indent-region-function #'cperl-indent-region)
  ;;(setq auto-fill-function #'cperl-do-auto-fill) ; Need to switch on and off!
  (setq-local imenu-create-index-function #'cperl-imenu--create-perl-index)
  (setq-local imenu-sort-function nil)
  (setq-local vc-rcs-header cperl-vc-rcs-header)
  (setq-local vc-sccs-header cperl-vc-sccs-header)
  (when (boundp 'compilation-error-regexp-alist-alist)
    ;; The let here is just a compatibility kludge for the obsolete
    ;; variable `cperl-compilation-error-regexp-alist'.  It can be removed
    ;; when that variable is removed.
    (let ((regexp (if (boundp 'cperl-compilation-error-regexp-alist)
                           (car cperl-compilation-error-regexp-alist)
                         cperl-compilation-error-regexp-list)))
      (setq-local compilation-error-regexp-alist-alist
                  (cons (cons 'cperl regexp)
                        compilation-error-regexp-alist-alist)))
    (make-local-variable 'compilation-error-regexp-alist)
    (push 'cperl compilation-error-regexp-alist))
  (setq-local font-lock-defaults
              '((cperl-load-font-lock-keywords
                 cperl-load-font-lock-keywords-1
                 cperl-load-font-lock-keywords-2)
                nil nil ((?_ . "w")) nil
                (font-lock-syntactic-face-function
                 . cperl-font-lock-syntactic-face-function)))
  ;; Reset syntaxification cache.
  (setq-local cperl-syntax-state nil)
  (when cperl-use-syntax-table-text-property
    ;; Reset syntaxification cache.
    (setq-local cperl-syntax-done-to nil)
    (setq-local syntax-propertize-function
                (lambda (start end)
                  (goto-char start)
                  ;; Even if cperl-fontify-syntactically has already gone
                  ;; beyond `start', syntax-propertize has just removed
                  ;; syntax-table properties between start and end, so we have
                  ;; to re-apply them.
                  (setq cperl-syntax-done-to start)
                  (cperl-fontify-syntactically end))))
  (setq-local font-lock-multiline t)
  (setq-local font-lock-fontify-region-function
              #'cperl-font-lock-fontify-region-function)
  (make-local-variable 'cperl-old-style)
  (setq-local normal-auto-fill-function
              #'cperl-do-auto-fill)
  (if (cperl-val 'cperl-font-lock)
      (progn (or cperl-faces-init (cperl-init-faces))
	     (font-lock-mode 1)))
  (setq-local facemenu-add-face-function
              #'cperl-facemenu-add-face-function) ; XXXX What this guy is for???
  (and (boundp 'msb-menu-cond)
       (not cperl-msb-fixed)
       (cperl-msb-fix))
  (if cperl-hook-after-change
      (add-hook 'after-change-functions #'cperl-after-change-function nil t))
  ;; After hooks since fontification will break this
  (when (and cperl-pod-here-scan
             (not cperl-syntaxify-by-font-lock))
    (cperl-find-pods-heres))
  (when cperl-file-style
    (cperl-set-style cperl-file-style))
  (add-hook 'hack-local-variables-hook #'cperl--set-file-style nil t)
  ;; Setup Flymake
  (add-hook 'flymake-diagnostic-functions #'perl-flymake nil t))

(when (fboundp 'derived-mode-add-parents) ; to run under Emacs <30
  (derived-mode-add-parents 'cperl-mode '(perl-mode)))

(defun cperl--set-file-style ()
  "Set the file style according to the variable `cperl-file-style'.
Do nothing if the variable is nil."
  (when cperl-file-style
    (cperl-file-style cperl-file-style)))


;; Fix for perldb - make default reasonable
(defun cperl-db ()
  "Obsolete workaround for an outdated issue with `perldb'."
  (declare (obsolete 'perldb "31.1"))
  (interactive)
  (require 'gud)
  ;; FIXME: Use `read-string' or `read-shell-command'?
  (perldb (read-from-minibuffer "Run perldb (like this): "
				(if (consp gud-perldb-history)
				    (car gud-perldb-history)
				  (concat "perl -d "
					  (buffer-file-name)))
				nil nil
				'(gud-perldb-history . 1))))

(defun cperl-msb-fix ()
  "Add perl files to msb menu, supposes that msb is already loaded."
  (setq cperl-msb-fixed t)
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(memq major-mode '(cperl-mode perl-mode))
		     handle
		     "Perl Files (%d)")
		    last))))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in CPerl code
;; based on its context.  Do fallback if comment is found wrong.

(defvar cperl-wrong-comment)
(defvar cperl-st-cfence '(14))		; Comment-fence
(defvar cperl-st-sfence '(15))		; String-fence
(defvar cperl-st-punct '(1))
(defvar cperl-st-word '(2))
(defvar cperl-st-bra '(4 . ?\>))
(defvar cperl-st-ket '(5 . ?\<))


(defun cperl-comment-indent ()
  "Called at point at supposed comment."
  (let ((p (point)) (c (current-column)) was phony)
    (if (and (not cperl-indent-comment-at-column-0)
	     (looking-at "^#"))
	0	; Existing comment at bol stays there.
      ;; Wrong comment found
      (save-excursion
	(setq was (cperl-to-comment-or-eol)
	      phony (eq (get-text-property (point) 'syntax-table)
			cperl-st-cfence))
	(if phony
	    (progn			; Too naive???
	      (re-search-forward "#\\|$") ; Hmm, what about embedded #?
	      (if (eq (preceding-char) ?\#)
		  (forward-char -1))
	      (setq was nil)))
	(if (= (point) p)		; Our caller found a correct place
	    (progn
	      (skip-chars-backward " \t")
	      (setq was (current-column))
	      (if (eq was 0)
		  comment-column
		(max (1+ was) ; Else indent at comment column
		     comment-column)))
	  ;; No, the caller found a random place; we need to edit ourselves
	  (if was nil
	    (insert comment-start)
	    (backward-char (length comment-start)))
	  (setq cperl-wrong-comment t)
	  (cperl-make-indent comment-column 1) ; Indent min 1
	  c)))))

(defun cperl-indent-for-comment ()
  "Substitute for `indent-for-comment' in CPerl."
  (interactive)
  (let (cperl-wrong-comment)
    (indent-for-comment)
    (if cperl-wrong-comment		; set by `cperl-comment-indent'
	(progn (cperl-to-comment-or-eol)
	       (forward-char (length comment-start))))))

(defun cperl-comment-region (b e arg)
  "Comment or uncomment each line in the region between B and E.
ARG is passed to `comment-region', which see."
  (interactive "r\np")
  (let ((comment-start "#"))
    (comment-region b e arg)))

(defun cperl-uncomment-region (b e arg)
  "Uncomment or comment each line in the region between B and E.
ARG is passed to `comment-region', which see."
  (interactive "r\np")
  (let ((comment-start "#"))
    (comment-region b e (- arg))))

(defvar cperl-brace-recursing nil)

(defun cperl-electric-brace (arg &optional only-before)
  "Insert character ARG and correct line's indentation.
If ONLY-BEFORE and `cperl-auto-newline', will insert newline before the
place (even in empty line), but not after.  If after \")\" and the inserted
char is \"{\", insert extra newline before only if
`cperl-extra-newline-before-brace'."
  (interactive "P")
  (let (insertpos
	(other-end (if (and cperl-electric-parens-mark
			    (region-active-p)
			    (< (mark) (point)))
		       (mark)
		     nil)))
    (if (and other-end
	     (not cperl-brace-recursing)
	     (cperl-val 'cperl-electric-parens)
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point)))
	;; Need to insert a matching pair
	(progn
	  (save-excursion
	    (setq insertpos (point-marker))
	    (goto-char other-end)
	    (setq last-command-event ?\{)
	    (cperl-electric-lbrace arg insertpos))
	  (forward-char 1))
      ;; Check whether we close something "usual" with `}'
      (if (and (eq last-command-event ?\})
	       (not
		(condition-case nil
		    (save-excursion
		      (up-list (- (prefix-numeric-value arg)))
		      ;;(cperl-after-block-p (point-min))
		      (or (cperl-after-expr-p nil "{;)")
			  ;; after sub, else, continue
			  (cperl-after-block-p nil 'pre)))
		  (error nil))))
	  ;; Just insert the guy
	  (self-insert-command (prefix-numeric-value arg))
	(if (and (not arg)		; No args, end (of empty line or auto)
		 (eolp)
		 (or (and (null only-before)
			  (save-excursion
			    (skip-chars-backward " \t")
			    (bolp)))
		     (and (eq last-command-event ?\{) ; Do not insert newline
			  ;; if after ")" and `cperl-extra-newline-before-brace'
			  ;; is nil, do not insert extra newline.
			  (not cperl-extra-newline-before-brace)
			  (save-excursion
			    (skip-chars-backward " \t")
			    (eq (preceding-char) ?\))))
		     (if cperl-auto-newline
			 (progn (cperl-indent-line) (newline) t) nil)))
	    (progn
	      (self-insert-command (prefix-numeric-value arg))
	      (cperl-indent-line)
	      (if cperl-auto-newline
		  (setq insertpos (1- (point))))
	      (if (and cperl-auto-newline (null only-before))
		  (progn
		    (newline)
		    (cperl-indent-line)))
	      (save-excursion
		(if insertpos (progn (goto-char insertpos)
				     (search-forward (make-string
						      1 last-command-event))
				     (setq insertpos (1- (point)))))
		(delete-char -1))))
	(if insertpos
	    (save-excursion
	      (goto-char insertpos)
	      (self-insert-command (prefix-numeric-value arg)))
	  (self-insert-command (prefix-numeric-value arg)))))))

(defun cperl-electric-lbrace (arg &optional end)
  "Insert character ARG, correct line's indentation, correct quoting by space.
Do not look beyond END."
  (interactive "P")
  (let ((cperl-brace-recursing t)
	(cperl-auto-newline cperl-auto-newline)
	(other-end (or end
		       (if (and cperl-electric-parens-mark
				(region-active-p)
				(> (mark) (point)))
			   (save-excursion
			     (goto-char (mark))
			     (point-marker))
			 nil)))
	pos)
    (and (cperl-val 'cperl-electric-lbrace-space)
	 (eq (preceding-char) ?$)
	 (save-excursion
	   (skip-chars-backward "$")
	   (looking-at "\\(\\$\\$\\)*\\$\\([^\\$]\\|$\\)"))
	 (insert ?\s))
    ;; Check whether we are in comment
    (if (and
	 (save-excursion
	   (beginning-of-line)
	   (not (looking-at "[ \t]*#")))
	 (cperl-after-expr-p nil "{;)"))
	nil
      (setq cperl-auto-newline nil))
    (cperl-electric-brace arg)
    (and (cperl-val 'cperl-electric-parens)
	 (eq last-command-event ?{)
	 (memq last-command-event
	       (append cperl-electric-parens-string nil))
	 (or (if other-end (goto-char (marker-position other-end)))
	     t)
	 (setq last-command-event ?} pos (point))
	 (progn (cperl-electric-brace arg t)
		(goto-char pos)))))

(defun cperl-electric-paren (arg)
  "Insert an opening parenthesis or a matching pair of parentheses.
See `cperl-electric-parens'.
Argument ARG is the opening parenthesis."
  (interactive "P")
  (let ((other-end (if (and cperl-electric-parens-mark
			    (region-active-p)
			    (> (mark) (point)))
		       (save-excursion
			 (goto-char (mark))
			 (point-marker))
		     nil)))
    (if (and (cperl-val 'cperl-electric-parens)
	     (memq last-command-event
		   (append cperl-electric-parens-string nil))
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point))
	     (if (eq last-command-event ?<)
		 (progn
		   ;; This code is too electric, see Bug#3943.
		   ;; (and abbrev-mode ; later it is too late, may be after `for'
		   ;; 	(expand-abbrev))
		   (cperl-after-expr-p nil "{;(,:="))
	       1))
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  (if other-end (goto-char (marker-position other-end)))
	  (insert (make-string
		   (prefix-numeric-value arg)
		   (cdr (assoc last-command-event '((?{ .?})
						   (?\[ . ?\])
						   (?\( . ?\))
						   (?< . ?>))))))
	  (forward-char (- (prefix-numeric-value arg))))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-rparen (arg)
  "Insert a matching pair of parentheses if marking is active.
If not, or if we are not at the end of marking range, would self-insert.
Affected by `cperl-electric-parens'.
Argument ARG is the closing parenthesis."
  (interactive "P")
  (let ((other-end (if (and cperl-electric-parens-mark
			    (cperl-val 'cperl-electric-parens)
			    (memq last-command-event
				  (append cperl-electric-parens-string nil))
			    (region-active-p)
			    (< (mark) (point)))
		       (mark)
		     nil))
	p)
    (if (and other-end
	     (cperl-val 'cperl-electric-parens)
	     (memq last-command-event '( ?\) ?\] ?\} ?\> ))
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point))
	     )
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  (setq p (point))
	  (if other-end (goto-char other-end))
	  (insert (make-string
		   (prefix-numeric-value arg)
		   (cdr (assoc last-command-event '((?\} . ?\{)
						   (?\] . ?\[)
						   (?\) . ?\()
						   (?\> . ?\<))))))
	  (goto-char (1+ p)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-keyword ()
  "Insert a construction appropriate after a keyword.
Help message may be switched off by setting `cperl-message-electric-keyword'
to nil."
  (let ((beg (line-beginning-position))
	(dollar (and (eq last-command-event ?$)
		     (eq this-command 'self-insert-command)))
	(delete (and (memq last-command-event '(?\s ?\n ?\t ?\f))
		     (memq this-command '(self-insert-command newline))))
	my do)
    (and (save-excursion
	   (condition-case nil
	       (progn
		 (backward-sexp 1)
		 (setq do (looking-at "do\\>")))
	     (error nil))
	   (cperl-after-expr-p nil "{;:"))
	 (save-excursion
	   (not
	    (re-search-backward
	     "[#\"'`]\\|\\<q\\(\\|[wqxr]\\)\\>"
	     beg t)))
	 (save-excursion (or (not (re-search-backward "^=" nil t))
			     (or
			      (looking-at "=cut")
			      (looking-at "=end")
			      (and cperl-use-syntax-table-text-property
				   (not (eq (get-text-property (point)
							       'syntax-type)
					    'pod))))))
	 (save-excursion (forward-sexp -1)
			 (not (memq (following-char) (append "$@%&*" nil))))
	 (progn
	   (and (eq (preceding-char) ?y)
		(progn			; "foreachmy"
		  (forward-char -2)
		  (insert " ")
		  (forward-char 2)
		  (setq my t dollar t
			delete
			(memq this-command '(self-insert-command newline)))))
	   (and dollar (insert " $"))
	   (cperl-indent-line)
	   ;;(insert " () {\n}")
 	   (cond
 	    (cperl-extra-newline-before-brace
 	     (insert (if do "\n" " ()\n"))
 	     (insert "{")
 	     (cperl-indent-line)
 	     (insert "\n")
 	     (cperl-indent-line)
 	     (insert "\n}")
	     (and do (insert " while ();")))
 	    (t
 	     (insert (if do " {\n} while ();" " () {\n}"))))
	   (or (looking-at "[ \t]\\|$") (insert " "))
	   (cperl-indent-line)
	   (if dollar (progn (search-backward "$")
			     (if my
				 (forward-char 1)
			       (delete-char 1)))
	     (search-backward ")")
	     (if (eq last-command-event ?\()
		 (progn			; Avoid "if (())"
		   (delete-char -1)
		   (delete-char 1))))
	   (if delete
               (push cperl-del-back-ch unread-command-events))
	   (if cperl-message-electric-keyword
	       (message "Precede char by C-q to avoid expansion"))))))

(defun cperl-ensure-newlines (n &optional pos)
  "Make sure there are N newlines after the point.
Go to POS which defaults to the current point after processing."
  (or pos (setq pos (point)))
  (if (looking-at "\n")
      (forward-char 1)
    (insert "\n"))
  (if (> n 1)
      (cperl-ensure-newlines (1- n) pos)
    (goto-char pos)))

(defun cperl-electric-pod ()
  "Insert a POD chunk appropriate after a =POD directive."
  (let ((delete (and (memq last-command-event '(?\s ?\n ?\t ?\f))
		     (memq this-command '(self-insert-command newline))))
	head1 notlast name p really-delete over)
    (and (save-excursion
	   (forward-word-strictly -1)
	   (and
	    (eq (preceding-char) ?=)
	    (progn
	      (setq head1 (looking-at "head1\\>[ \t]*$"))
	      (setq over (and (looking-at "over\\>[ \t]*$")
			      (not (looking-at "over[ \t]*\n\n\n*=item\\>"))))
	      (forward-char -1)
	      (bolp))
	    (or
	     (get-text-property (point) 'in-pod)
	     (cperl-after-expr-p nil "{;:")
	     (and (re-search-backward "\\(\\`\n?\\|^\n\\)=\\sw+" (point-min) t)
		  (not (or (looking-at "\n*=cut") (looking-at "\n*=end")))
		  (or (not cperl-use-syntax-table-text-property)
		      (eq (get-text-property (point) 'syntax-type) 'pod))))))
	 (progn
	   (save-excursion
	     (setq notlast (re-search-forward "^\n=" nil t)))
	   (or notlast
	       (progn
		 (insert "\n\n=cut")
		 (cperl-ensure-newlines 2)
		 (forward-word-strictly -2)
		 (if (and head1
			  (not
			   (save-excursion
			     (forward-char -1)
			     (re-search-backward "\\(\\`\n?\\|\n\n\\)=head1\\>"
						 nil t)))) ; Only one
		     (progn
		       (forward-word-strictly 1)
		       (setq name (file-name-base (buffer-file-name))
			     p (point))
		       (insert " NAME\n\n" name
			       " - \n\n=head1 SYNOPSIS\n\n\n\n"
			       "=head1 DESCRIPTION")
		       (cperl-ensure-newlines 4)
		       (goto-char p)
		       (forward-word-strictly 2)
		       (end-of-line)
		       (setq really-delete t))
		   (forward-word-strictly 1))))
	   (if over
	       (progn
		 (setq p (point))
		 (insert "\n\n=item \n\n\n\n"
			 "=back")
		 (cperl-ensure-newlines 2)
		 (goto-char p)
		 (forward-word-strictly 1)
		 (end-of-line)
		 (setq really-delete t)))
	   (if (and delete really-delete)
               (push cperl-del-back-ch unread-command-events))))))

(defun cperl-electric-else ()
  "Insert a construction appropriate after a keyword.
Help message may be switched off by setting `cperl-message-electric-keyword'
to nil."
  (let ((beg (line-beginning-position)))
    (and (save-excursion
           (skip-chars-backward "[:alpha:]")
	   (cperl-after-expr-p nil "{;:"))
	 (save-excursion
	   (not
	    (re-search-backward
	     "[#\"'`]\\|\\<q\\(\\|[wqxr]\\)\\>"
	     beg t)))
	 (save-excursion (or (not (re-search-backward "^=" nil t))
			     (looking-at "=cut")
			     (looking-at "=end")
			     (and cperl-use-syntax-table-text-property
				  (not (eq (get-text-property (point)
							      'syntax-type)
					   'pod)))))
	 (progn
	   (cperl-indent-line)
	   ;;(insert " {\n\n}")
 	   (cond
 	    (cperl-extra-newline-before-brace
 	     (insert "\n")
 	     (insert "{")
 	     (cperl-indent-line)
 	     (insert "\n\n}"))
 	    (t
 	     (insert " {\n\n}")))
	   (or (looking-at "[ \t]\\|$") (insert " "))
	   (cperl-indent-line)
	   (forward-line -1)
	   (cperl-indent-line)
           (push cperl-del-back-ch unread-command-events)
	   (setq this-command 'cperl-electric-else)
	   (if cperl-message-electric-keyword
	       (message "Precede char by C-q to avoid expansion"))))))

(defun cperl-linefeed ()
  "Go to end of line, open a new line and indent appropriately.
If in POD, insert appropriate lines."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
	(pos (point)) start over cut res)
    (if (and				; Check if we need to split:
					; i.e., on a boundary and inside "{...}"
	 (save-excursion (cperl-to-comment-or-eol)
			 (>= (point) pos)) ; Not in a comment
	 (or (save-excursion
	       (skip-chars-backward " \t" beg)
	       (forward-char -1)
	       (looking-at "[;{]"))     ; After { or ; + spaces
	     (looking-at "[ \t]*}")	; Before }
	     (re-search-forward "\\=[ \t]*;" end t)) ; Before spaces + ;
	 (save-excursion
	   (and
	    (eq (car (parse-partial-sexp pos end -1)) -1)
					; Leave the level of parens
	    (looking-at "[,; \t]*\\($\\|#\\)") ; Comma to allow anon subr
					; Are at end
	    (cperl-after-block-p (point-min))
	    (progn
	      (backward-sexp 1)
	      (setq start (point-marker))
	      (<= start pos)))))	; Redundant?  Are after the
					; start of parens group.
	(progn
	  (skip-chars-backward " \t")
	  (or (memq (preceding-char) (append ";{" nil))
	      (insert ";"))
	  (insert "\n")
	  (forward-line -1)
	  (cperl-indent-line)
	  (goto-char start)
	  (or (looking-at "{[ \t]*$")	; If there is a statement
					; before, move it to separate line
	      (progn
		(forward-char 1)
		(insert "\n")
		(cperl-indent-line)))
	  (forward-line 1)		; We are on the target line
	  (cperl-indent-line)
	  (beginning-of-line)
	  (or (looking-at "[ \t]*}[,; \t]*$") ; If there is a statement
					; after, move it to separate line
	      (progn
		(end-of-line)
		(search-backward "}" beg)
		(skip-chars-backward " \t")
		(or (memq (preceding-char) (append ";{" nil))
		    (insert ";"))
		(insert "\n")
		(cperl-indent-line)
		(forward-line -1)))
	  (forward-line -1)		; We are on the line before target
	  (end-of-line)
	  (newline-and-indent))
      (end-of-line)			; else - no splitting
      (cond
       ((and (looking-at "\n[ \t]*{$")
	     (save-excursion
	       (skip-chars-backward " \t")
	       (eq (preceding-char) ?\)))) ; Probably if () {} group
					; with an extra newline.
	(forward-line 2)
	(cperl-indent-line))
       ((save-excursion			; In POD header
	  (forward-paragraph -1)
	  ;; (re-search-backward "\\(\\`\n?\\|\n\n\\)=head1\\b")
	  ;; We are after \n now, so look for the rest
	  (if (looking-at "\\(\\`\n?\\|\n\\)=\\sw+")
	      (progn
		(setq cut (looking-at "\\(\\`\n?\\|\n\\)=\\(cut\\|end\\)\\>"))
		(setq over (looking-at "\\(\\`\n?\\|\n\\)=over\\>"))
		t)))
	(if (and over
		 (progn
		   (forward-paragraph -1)
		   (forward-word-strictly 1)
		   (setq pos (point))
                   (setq cut (buffer-substring (point) (line-end-position)))
                   (delete-char (- (line-end-position) (point)))
		   (setq res (expand-abbrev))
		   (save-excursion
		     (goto-char pos)
		     (insert cut))
		   res))
	    nil
	  (cperl-ensure-newlines (if cut 2 4))
	  (forward-line 2)))
       ((get-text-property (point) 'in-pod) ; In POD section
	(cperl-ensure-newlines 4)
	(forward-line 2))
       ((looking-at "\n[ \t]*$")	; Next line is empty - use it.
        (forward-line 1)
	(cperl-indent-line))
       (t
	(newline-and-indent))))))

(defun cperl-electric-semi (arg)
  "Insert character and correct line's indentation.
ARG is the character to insert."
  (interactive "P")
  (if cperl-auto-newline
      (cperl-electric-terminator arg)
    (self-insert-command (prefix-numeric-value arg))
    (if cperl-autoindent-on-semi
	(cperl-indent-line))))

(defun cperl-electric-terminator (arg)
  "Insert character and correct line's indentation.
ARG is the character to insert."
  (interactive "P")
  (let ((end (point))
	(auto (and cperl-auto-newline
		   (or (not (eq last-command-event ?:))
		       cperl-auto-newline-after-colon)))
	insertpos)
    (if (and ;;(not arg)
	     (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or
		     ;; Ignore in comment lines
		     (= (following-char) ?#)
		     ;; Colon is special only after a label
		     ;; So quickly rule out most other uses of colon
		     ;; and do no indentation for them.
		     (and (eq last-command-event ?:)
			  (save-excursion
			    (forward-word-strictly 1)
			    (skip-chars-forward " \t")
			    (and (< (point) end)
				 (progn (goto-char (- end 1))
					(not (looking-at ":"))))))
		     (progn
		       (beginning-of-defun)
		       (let ((pps (parse-partial-sexp (point) end)))
			 (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  ;;(forward-char -1)
	  (if auto (setq insertpos (point-marker)))
	  ;;(forward-char 1)
	  (cperl-indent-line)
	  (if auto
	      (progn
		(newline)
		(cperl-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1- (marker-position insertpos)))
	      (forward-char -1))
	    (delete-char 1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-backspace (arg)
  "Backspace, or remove whitespace around the point inserted by an electric key.
Will untabify if `cperl-electric-backspace-untabify' is non-nil.
ARG is the key which caused the action."
  (interactive "p")
  (if (and cperl-auto-newline
	   (memq last-command '(cperl-electric-semi
				cperl-electric-terminator
				cperl-electric-lbrace))
	   (memq (preceding-char) '(?\s ?\t ?\n)))
      (let (p)
	(if (eq last-command 'cperl-electric-lbrace)
	    (skip-chars-forward " \t\n"))
	(setq p (point))
	(skip-chars-backward " \t\n")
	(delete-region (point) p))
    (and (eq last-command 'cperl-electric-else)
	 ;; We are removing the whitespace *inside* cperl-electric-else
	 (setq this-command 'cperl-electric-else-really))
    (if (and cperl-auto-newline
	     (eq last-command 'cperl-electric-else-really)
	     (memq (preceding-char) '(?\s ?\t ?\n)))
	(let (p)
	  (skip-chars-forward " \t\n")
	  (setq p (point))
	  (skip-chars-backward " \t\n")
	  (delete-region (point) p))
      (if cperl-electric-backspace-untabify
	  (backward-delete-char-untabify arg)
	(call-interactively 'delete-backward-char)))))

(put 'cperl-electric-backspace 'delete-selection 'supersede)

(defun cperl-inside-parens-p ()
  "Obsolete function, unused."
  (declare (obsolete nil "28.1")) ; not used
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-defun) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

(defun cperl-indent-command (&optional whole-exp)
  "Indent current line as Perl code, or in some cases insert a tab character.
If `cperl-tab-always-indent' is non-nil (the default), always indent current
line.  Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved.
If WHOLE-EXP is non-nil, indent the whole expression."
  (interactive "P")
  (cperl-update-syntaxification (point))
  (if whole-exp
      ;; If arg, always indent this line as Perl
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (cperl-indent-line))
	    beg end)
	(save-excursion
	  (if cperl-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (and shift-amt (> end beg))
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not cperl-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (cperl-indent-line))))

(defun cperl-indent-line (&optional parse-data)
  "Indent current line as Perl code.
Return the amount the indentation changed by.
PARSE-DATA is used to save status between calls in a loop."
  (let ((case-fold-search nil)
	(pos (- (point-max) (point)))
	indent i shift-amt)
    (setq indent (cperl-calculate-indent parse-data)
	  i indent)
    (beginning-of-line)
    (cond ((or (eq indent nil) (eq indent t))
	   (setq indent (current-indentation) i nil))
	  ;;((eq indent t)    ; Never?
	  ;; (setq indent (cperl-calculate-indent-within-comment)))
	  ;;((looking-at "[ \t]*#")
	  ;; (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((and (looking-at (rx (sequence (eval cperl--label-rx)
                                                 (not (in ":")))))
                       (null (get-text-property (point) 'syntax-type))
                       (not (looking-at (rx (eval cperl--false-label-rx)))))
		  (and (> indent 0)
		       (setq indent (max cperl-min-label-indent
					 (+ indent cperl-label-offset)))))
		 ((= (following-char) ?})
		  (setq indent (- indent cperl-indent-level)))
		 ((memq (following-char) '(?\) ?\])) ; To line up with opening paren.
		  (setq indent (+ indent cperl-close-paren-offset)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent cperl-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (and i (- indent (current-column))))
    (if (or (not shift-amt)
	    (zerop shift-amt))
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      ;;(delete-region beg (point))
      ;;(indent-to indent)
      (cperl-make-indent indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun cperl-after-label ()
  "Return non-nil if the point is after label.  Does not do `save-excursion'."
  (and (eq (preceding-char) ?:)
       (memq (char-syntax (char-after (- (point) 2)))
	     '(?w ?_))
       (progn
	 (backward-sexp)
         (looking-at (rx (sequence (eval cperl--label-rx)
                                   (not (in ":"))))))))

(defun cperl-get-state (&optional parse-start start-state)
  "Return list (START STATE DEPTH PRESTART).
START is a good place to start parsing, or equal to
PARSE-START if preset.
STATE is what is returned by `parse-partial-sexp'.
DEPTH is true if we are immediately after end of block
which contains START.
PRESTART is the position basing on which START was found.
START-STATE should be a good guess for the start of a function."
  (save-excursion
    (let ((start-point (point)) depth state start prestart)
      (if (and parse-start
	       (<= parse-start start-point))
	  (goto-char parse-start)
	(beginning-of-defun-raw)
        (when (cperl-declaration-header-p (point))
          (goto-char (cperl-beginning-of-property (point) 'syntax-type))
          (beginning-of-line))
	(setq start-state nil))
      (setq prestart (point))
      (if start-state nil
	;; Try to go out, if sub is not on the outermost level
	(while (< (point) start-point)
	  (setq start (point) parse-start start depth nil
		state (parse-partial-sexp start start-point -1))
	  (if (> (car state) -1) nil
	    ;; The current line could start like }}}, so the indentation
	    ;; corresponds to a different level than what we reached
	    (setq depth t)
	    (beginning-of-line 2)))	; Go to the next line.
	(if start (goto-char start)))	; Not at the start of file
      (setq start (point))
      (or state (setq state (parse-partial-sexp start start-point -1 nil start-state)))
      (list start state depth prestart))))

(defvar cperl-look-for-prop '((pod in-pod) (here-doc-delim here-doc-group)))

(defun cperl-beginning-of-property (p prop &optional lim)
  "Given that P has a property PROP, find where the property start.
Will not look before LIM."
;; XXXX What to do at point-max???
  (or (previous-single-property-change (cperl-1+ p) prop lim)
      (point-min))
  ;; (cond ((eq p (point-min))
  ;;        p)
  ;;       ((and lim (<= p lim))
  ;;        p)
  ;;       ((not (get-text-property (1- p) prop))
  ;;        p)
  ;;       (t (or (previous-single-property-change p look-prop lim)
  ;;              (point-min))))
  )

(defun cperl-sniff-for-indent (&optional parse-data) ; was parse-start
  "Find out what the current line means, based on the given PARSE-DATA."
  (cperl-update-syntaxification (point))
  (let ((res (get-text-property (point) 'syntax-type)))
    (save-excursion
      (cond
       ((and (memq res '(pod here-doc here-doc-delim format))
	     (not (get-text-property (point) 'indentable)))
	(vector res))
       ;; before start of POD - whitespace found since do not have 'pod!
       ((looking-at "[ \t]*\n=")
	(error "Spaces before POD section!"))
       ((and (not cperl-indent-left-aligned-comments)
	     (looking-at "^#"))
	[comment-special:at-beginning-of-line])
       ((get-text-property (point) 'in-pod)
	[in-pod])
       (t
	(beginning-of-line)
	(let* ((indent-point (point))
	       (char-after-pos (save-excursion
				 (skip-chars-forward " \t")
				 (point)))
	       (char-after (char-after char-after-pos))
	       (pre-indent-point (point))
	       p prop look-prop is-block delim)
	  (save-excursion		; Know we are not in POD, find appropriate pos before
	    (cperl-backward-to-noncomment nil)
	    (setq p (max (point-min) (1- (point)))
		  prop (get-text-property p 'syntax-type)
		  look-prop (or (nth 1 (assoc prop cperl-look-for-prop))
				'syntax-type))
	    (if (memq prop '(pod here-doc format here-doc-delim))
		(progn
		  (goto-char (cperl-beginning-of-property p look-prop))
		  (beginning-of-line)
		  (setq pre-indent-point (point)))))
	  (goto-char pre-indent-point)	; Orig line skipping preceding pod/etc
	  (let* ((case-fold-search nil)
		 (s-s (cperl-get-state (car parse-data) (nth 1 parse-data)))
		 (start (or (nth 2 parse-data) ; last complete sexp terminated
			    (nth 0 s-s))) ; Good place to start parsing
		 (state (nth 1 s-s))
		 (containing-sexp (car (cdr state)))
		 old-indent)
	    (if (and
		 ;;containing-sexp		;; We are buggy at toplevel :-(
		 parse-data)
		(progn
		  (setcar parse-data pre-indent-point)
		  (setcar (cdr parse-data) state)
		  (or (nth 2 parse-data)
		      (setcar (cddr parse-data) start))
		  ;; Before this point: end of statement
		  (setq old-indent (nth 3 parse-data))))
	    (cond ((get-text-property (point) 'indentable)
		   ;; indent to "after" the surrounding open
		   ;; (same offset as `cperl-beautify-regexp-piece'),
		   ;; skip blanks if we do not close the expression.
		   (setq delim		; We do not close the expression
			 (get-text-property
			  (cperl-1+ char-after-pos) 'indentable)
			 p (1+ (cperl-beginning-of-property
				(point) 'indentable))
			 is-block	; misused for: preceding line in REx
			 (save-excursion ; Find preceding line
			   (cperl-backward-to-noncomment p)
			   (beginning-of-line)
			   (if (<= (point) p)
			       (progn	; get indent from the first line
				 (goto-char p)
				 (skip-chars-forward " \t")
				 (if (memq (char-after (point))
					   (append "#\n" nil))
				     nil ; Can't use indentation of this line...
				   (point)))
			     (skip-chars-forward " \t")
			     (point)))
			 prop (parse-partial-sexp p char-after-pos))
		   (cond ((not delim)	; End the REx, ignore is-block
			  (vector 'indentable 'terminator p is-block))
			 (is-block	; Indent w.r.t. preceding line
			  (vector 'indentable 'cont-line char-after-pos
				  is-block char-after p))
			 (t		; No preceding line...
			  (vector 'indentable 'first-line p))))
		  ((get-text-property char-after-pos 'REx-part2)
		   (vector 'REx-part2 (point)))
		  ((nth 4 state)
		   [comment])
		  ((nth 3 state)
		   [string])
		  ;; XXXX Do we need to special-case this?
		  ((null containing-sexp)
		   ;; Line is at top level.  May be data or function definition,
		   ;; or may be function argument declaration.
		   ;; Indent like the previous top level line
		   ;; unless that ends in a closeparen without semicolon,
		   ;; in which case this line is the first argument decl.
		   (skip-chars-forward " \t")
		   (cperl-backward-to-noncomment (or old-indent (point-min)))
                   ;; Determine whether point is between statements
		   (setq state
			 (or (bobp)
			     (eq (point) old-indent) ; old-indent was at comment
			     (eq (preceding-char) ?\;)
			     ;;  Had ?\) too
			     (and (eq (preceding-char) ?\})
				  (cperl-after-block-and-statement-beg
				   (point-min))) ; Was start - too close
                             (and char-after (char-equal char-after ?{)
                                  (save-excursion (cperl-block-declaration-p)))
			     (memq char-after (append ")]}" nil))
			     (and (eq (preceding-char) ?\:) ; label
				  (progn
				    (forward-sexp -1)
				    (skip-chars-backward " \t")
				    (looking-at
                                     (rx (sequence (0+ blank)
                                                   (eval cperl--label-rx))))))
			     (get-text-property (1- (point)) 'first-format-line)
                             (equal (get-text-property (point) 'syntax-type) 'format)))

		   ;; Look at previous line that's at column 0
		   ;; to determine whether we are in top-level decls
		   ;; or function's arg decls.  Set basic-indent accordingly.
		   ;; Now add a little if this is a continuation line.
		   (and state
			parse-data
			(not (eq char-after ?\C-j))
			(setcdr (cddr parse-data)
				(list pre-indent-point)))
		   (vector 'toplevel start char-after state (nth 2 s-s)))
		  ((not
		    (or (setq is-block
			      (and (setq delim (= (char-after containing-sexp) ?{))
				   (save-excursion ; Is it a hash?
				     (goto-char containing-sexp)
				     (cperl-block-p))))
			cperl-indent-parens-as-block))
		   ;; group is an expression, not a block:
		   ;; indent to just after the surrounding open parens,
		   ;; skip blanks if we do not close the expression.
		   (goto-char (1+ containing-sexp))
		   (or (memq char-after
			     (append (if delim "}" ")]}") nil))
		       (looking-at "[ \t]*\\(#\\|$\\)")
		       (skip-chars-forward " \t"))
		   (setq old-indent (point)) ; delim=is-brace
		   (vector 'in-parens char-after (point) delim containing-sexp))
		  (t
		   ;; Statement level.  Is it a continuation or a new statement?
		   ;; Find previous non-comment character.
		   (goto-char pre-indent-point) ; Skip one level of POD/etc
		   (cperl-backward-to-noncomment containing-sexp)
		   ;; Back up over label lines, since they don't
		   ;; affect whether our line is a continuation.
		   ;; (Had \, too)
                   (while (save-excursion
                            (and (eq (preceding-char) ?:)
                                 (re-search-backward
                                  (rx (sequence (eval cperl--label-rx) point))
                                  nil t)
                                 ;; Ignore if in comment or RE
                                 (not (nth 3 (syntax-ppss)))))
		     ;; This is always FALSE?
		     (if (eq (preceding-char) ?\,)
			 ;; Will go to beginning of line, essentially.
			 ;; Will ignore embedded sexpr XXXX.
			 (cperl-backward-to-start-of-continued-exp containing-sexp))
		     (beginning-of-line)
		     (cperl-backward-to-noncomment containing-sexp))
		   ;; Now we get non-label preceding the indent point
		   (if (not (or (eq (1- (point)) containing-sexp)
                                (and cperl-indent-parens-as-block
                                     (not is-block))
                                (and (looking-at "{")
                                     (save-excursion (cperl-block-declaration-p)))
				(memq (preceding-char)
				      (append (if is-block " ;{" " ,;{") '(nil)))
				(and (eq (preceding-char) ?\})
				     (cperl-after-block-and-statement-beg
				      containing-sexp))
                                (and (cperl-declaration-header-p indent-point)
                                     (not (cperl-declaration-header-p char-after-pos)))
				(get-text-property (point) 'first-format-line)))
		       ;; This line is continuation of preceding line's statement;
		       ;; indent  `cperl-continued-statement-offset'  more than the
		       ;; previous line of the statement.
		       ;;
		       ;; There might be a label on this line, just
		       ;; consider it bad style and ignore it.
		       (progn
			 (cperl-backward-to-start-of-continued-exp containing-sexp)
			 (vector 'continuation (point) char-after is-block delim))
		     ;; This line starts a new statement.
		     ;; Position following last unclosed open brace
		     (goto-char containing-sexp)
		     ;; Is line first statement after an open-brace?
		     (or
		      ;; If no, find that first statement and indent like
		      ;; it.  If the first statement begins with label, do
		      ;; not believe when the indentation of the label is too
		      ;; small.
		      (save-excursion
			(forward-char 1)
			(let ((colon-line-end 0))
			  (while
			      (progn
                                (skip-chars-forward " \t\n")
				;; s: foo : bar :x is NOT label
                                (and (looking-at
                                      (rx
                                       (or "#"
                                           (sequence (eval cperl--label-rx)
                                                     (not (in ":")))
                                           (sequence "=" (in "a-zA-Z")))))
				     (not (looking-at
                                           (rx (eval cperl--false-label-rx))))))
			    ;; Skip over comments and labels following openbrace.
			    (cond ((= (following-char) ?\#)
				   (forward-line 1))
				  ((= (following-char) ?\=)
				   (goto-char
				    (or (next-single-property-change (point) 'in-pod)
					(point-max)))) ; do not loop if no syntaxification
				  ;; label:
				  (t
                                   (setq colon-line-end (line-end-position))
				   (search-forward ":"))))
			  ;; We are at beginning of code (NOT label or comment)
			  ;; First, the following code counts
			  ;; if it is before the line we want to indent.
			  (and (< (point) indent-point)
			       (vector 'have-prev-sibling (point) colon-line-end
				       containing-sexp))))
		      (progn
			;; If no previous statement,
			;; indent it relative to line brace is on.

			;; For open-braces not the first thing in a line,
			;; add in cperl-brace-imaginary-offset.

			;; If first thing on a line:  ?????
			;; Move back over whitespace before the openbrace.
			(setq		; brace first thing on a line
			 old-indent (progn (skip-chars-backward " \t") (bolp)))
			;; Should we indent w.r.t. earlier than start?
			;; Move to start of control group, possibly on a different line
			(or cperl-indent-wrt-brace
			    (cperl-backward-to-noncomment (point-min)))
			;; If the openbrace is preceded by a parenthesized exp,
			;; move to the beginning of that;
			(if (eq (preceding-char) ?\))
			    (progn
			      (forward-sexp -1)
			      (cperl-backward-to-noncomment (point-min))))
			;; In the case it starts a subroutine, indent with
			;; respect to `sub', not with respect to the
			;; first thing on the line, say in the case of
			;; anonymous sub in a hash.
			(if (and;; Is it a sub in group starting on this line?
                             cperl-indent-subs-specially
			     (cond
                              ((cperl-declaration-header-p (point))
                               (goto-char
                                (cperl-beginning-of-property (point)
                                                             'syntax-type))))
			     (setq p (nth 1 ; start of innermost containing list
					  (parse-partial-sexp
                                           (line-beginning-position)
					   (point)))))
			    (progn
			      (goto-char (1+ p)) ; enclosing block on the same line
			      (skip-chars-forward " \t")
			      (vector 'code-start-in-block containing-sexp char-after
				      (and delim (not is-block)) ; is a HASH
				      old-indent ; brace first thing on a line
				      t (point) ; have something before...
				      )
			      ;;(current-column)
			      )
			  ;; Get initial indentation of the line we are on.
			  ;; If line starts with label, calculate label indentation
			  (vector 'code-start-in-block containing-sexp char-after
				  (and delim (not is-block)) ; is a HASH
				  old-indent ; brace first thing on a line
				  nil (point))))))))))))))) ; nothing interesting before

(defvar cperl-indent-rules-alist
  '((pod nil)				; via `syntax-type' property
    (here-doc nil)			; via `syntax-type' property
    (here-doc-delim nil)		; via `syntax-type' property
    (format nil)			; via `syntax-type' property
    (in-pod nil)			; via `in-pod' property
    (comment-special:at-beginning-of-line nil)
    (string t)
    (comment nil))
  "Alist of indentation rules for CPerl mode.
The values mean:
  nil: do not indent;
  FUNCTION: a function to compute the indentation to use.
    Takes a single argument which provides the currently computed indentation
    context, and should return the column to which to indent.
  NUMBER: add this amount of indentation.")

(defun cperl-calculate-indent (&optional parse-data) ; was parse-start
  "Return appropriate indentation for current line as Perl code.
PARSE-DATA is the result of a previous call to speed up things.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.

Will not correct the indentation for labels, but will correct it for braces
and closing parentheses and brackets."
  ;; This code is still a broken architecture: in some cases we need to
  ;; compensate for some modifications which `cperl-indent-line' will add later
  (save-excursion
    (let ((i (cperl-sniff-for-indent parse-data)) what p)
      (cond
       ;;((or (null i) (eq i t) (numberp i))
       ;;  i)
       ((vectorp i)
	(setq what (assoc (elt i 0) cperl-indent-rules-alist))
	(cond
         (what
          (let ((action (cadr what)))
            (cond ((functionp action) (apply action (list i parse-data)))
                  ((numberp action) (+ action (current-indentation)))
                  (t action))))
	 ;;
	 ;; Indenters for regular expressions with //x and qw()
	 ;;
	 ((eq 'REx-part2 (elt i 0)) ;; [self start] start of /REP in s//REP/x
	  (goto-char (elt i 1))
          (condition-case nil
	      (forward-sexp -1) 	; Use indentation of the 1st part
            (error nil))
	  (current-column))
	 ((eq 'indentable (elt i 0))	; Indenter for REGEXP qw() etc
	  (cond		       ; [indentable terminator start-pos is-block]
	   ((eq 'terminator (elt i 1)) ; Lone terminator of "indentable string"
	    (goto-char (elt i 2))	; After opening parens
	    (1- (current-column)))
	   ((eq 'first-line (elt i 1)); [indentable first-line start-pos]
	    (goto-char (elt i 2))
	    (+ (or cperl-regexp-indent-step cperl-indent-level)
	       -1
	       (current-column)))
	   ((eq 'cont-line (elt i 1)); [indentable cont-line pos prev-pos first-char start-pos]
	    ;; Indent as the level after closing parens
	    (goto-char (elt i 2))	; indent line
	    (skip-chars-forward " \t)") ; Skip closing parens
	    (setq p (point))
	    (goto-char (elt i 3))	; previous line
	    (skip-chars-forward " \t)") ; Skip closing parens
	    ;; Number of parens in between:
	    (setq p (nth 0 (parse-partial-sexp (point) p))
		  what (elt i 4))	; First char on current line
	    (goto-char (elt i 3))	; previous line
	    (+ (* p (or cperl-regexp-indent-step cperl-indent-level))
	       (cond ((eq what ?\) )
		      (- cperl-close-paren-offset)) ; compensate
		     ((eq what ?\| )
		      (- (or cperl-regexp-indent-step cperl-indent-level)))
		     (t 0))
	       (if (eq (following-char) ?\| )
		   (or cperl-regexp-indent-step cperl-indent-level)
		 0)
	       (current-column)))
	   (t
	    (error "Unrecognized value of indent: %s" i))))
	 ;;
	 ;; Indenter for stuff at toplevel
	 ;;
	 ((eq 'toplevel (elt i 0)) ;; [toplevel start char-after state immed-after-block]
	  (+ (save-excursion		; To beg-of-defun, or end of last sexp
	       (goto-char (elt i 1))	; start = Good place to start parsing
	       (- (current-indentation) ;
		  (if (elt i 4) cperl-indent-level 0)))	; immed-after-block
	     (if (eq (elt i 2) ?{) cperl-continued-brace-offset 0) ; char-after
	     ;; Look at previous line that's at column 0
	     ;; to determine whether we are in top-level decls
	     ;; or function's arg decls.  Set basic-indent accordingly.
	     ;; Now add a little if this is a continuation line.
	     (if (elt i 3)		; state (XXX What is the semantic???)
		 0
	       cperl-continued-statement-offset)))
	 ;;
	 ;; Indenter for stuff in "parentheses" (or brackets, braces-as-hash)
	 ;;
	 ((eq 'in-parens (elt i 0))
	  ;; in-parens char-after old-indent-point is-brace containing-sexp

	  ;; group is an expression, not a block:
	  ;; indent to just after the surrounding open parens,
	  ;; skip blanks if we do not close the expression.
	  (+ (progn
	       (goto-char (elt i 2))		; old-indent-point
	       (current-column))
	     (if (and (elt i 3)		; is-brace
		      (eq (elt i 1) ?\})) ; char-after
		 ;; Correct indentation of trailing ?\}
		 (+ cperl-indent-level cperl-close-paren-offset)
	       0)))
	 ;;
	 ;; Indenter for continuation lines
	 ;;
	 ((eq 'continuation (elt i 0))
	  ;; [continuation statement-start char-after is-block is-brace]
	  (goto-char (elt i 1))		; statement-start
	  (+ (if (or (memq (elt i 2) (append "}])" nil)) ; char-after
                     (eq 'continuation ; do not stagger continuations
                         ;; FIXME: This clobbers the syntax state in parse-data
                         ;; for the *following* lines and makes the state
                         ;; useless for indent-region -- haj 2023-06-30
                         (elt (cperl-sniff-for-indent parse-data) 0)))
		 0 ; Closing parenthesis or continuation of a continuation
	       cperl-continued-statement-offset)
	     (if (or (elt i 3)		; is-block
		     (not (elt i 4))		; is-brace
		     (not (eq (elt i 2) ?\}))) ; char-after
		 0
	       ;; Now it is a hash reference
	       (+ cperl-indent-level cperl-close-paren-offset))
	     ;; Labels do not take :: ...
	     (if (and (looking-at "\\(\\w\\|_\\)+[ \t]*:[^:]")
                      (not (looking-at (rx (eval cperl--false-label-rx)))))
		 (if (> (current-indentation) cperl-min-label-indent)
		     (- (current-indentation) cperl-label-offset)
		   ;; Do not move `parse-data', this should
		   ;; be quick anyway (this comment comes
		   ;; from different location):
		   (cperl-calculate-indent))
	       (current-column))
	     (if (eq (elt i 2) ?\{)	; char-after
		 cperl-continued-brace-offset 0)))
	 ;;
	 ;; Indenter for lines in a block which are not leading lines
	 ;;
	 ((eq 'have-prev-sibling (elt i 0))
	  ;; [have-prev-sibling sibling-beg colon-line-end block-start]
	  (goto-char (elt i 1))		; sibling-beg
	  (if (> (elt i 2) (point)) ; colon-line-end; have label before point
	      (if (> (current-indentation)
		     cperl-min-label-indent)
		  (- (current-indentation) cperl-label-offset)
		;; Do not believe: `max' was involved in calculation of indent
		(+ cperl-indent-level
		   (save-excursion
		     (goto-char (elt i 3)) ; block-start
		     (current-indentation))))
	    (current-column)))
	 ;;
	 ;; Indenter for the first line in a block
	 ;;
	 ((eq 'code-start-in-block (elt i 0))
	  ;;[code-start-in-block before-brace char-after
	  ;; is-a-HASH-ref brace-is-first-thing-on-a-line
	  ;; group-starts-before-start-of-sub start-of-control-group]
	  (goto-char (elt i 1))
	  ;; For open brace in column zero, don't let statement
	  ;; start there too.  If cperl-indent-level=0,
	  ;; use cperl-brace-offset + cperl-continued-statement-offset instead.
	  (+ (if (and (bolp) (zerop cperl-indent-level))
		 (+ cperl-brace-offset cperl-continued-statement-offset)
	       cperl-indent-level)
	     (if (and (elt i 3)	; is-a-HASH-ref
		      (eq (elt i 2) ?\})) ; char-after: End of a hash reference
		 (+ cperl-indent-level cperl-close-paren-offset)
	       0)
	     ;; Unless openbrace is the first nonwhite thing on the line,
	     ;; add the cperl-brace-imaginary-offset.
	     (if (elt i 4) 0		; brace-is-first-thing-on-a-line
	       cperl-brace-imaginary-offset)
	     (progn
	       (goto-char (elt i 6))	; start-of-control-group
	       (if (elt i 5)		; group-starts-before-start-of-sub
		   (current-column)
		 ;; Get initial indentation of the line we are on.
		 ;; If line starts with label, calculate label indentation
		 (if (save-excursion
		       (beginning-of-line)
                       (looking-at (rx
                                    (sequence (0+ space)
                                              (eval cperl--label-rx)
                                              (not (in ":"))))))
		     (if (> (current-indentation) cperl-min-label-indent)
			 (- (current-indentation) cperl-label-offset)
		       ;; Do not move `parse-data', this should
		       ;; be quick anyway:
		       (cperl-calculate-indent))
		   (current-indentation))))))
	 (t
	  (error "Unrecognized value of indent: %s" i))))
       (t
	(error "Got strange value of indent: %s" i))))))

(defun cperl-calculate-indent-within-comment ()
  "Return the indentation amount for line.
Assume that the current line is to be regarded as part of a block
comment."
  (let (end)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq end (point))
      (and (= (following-char) ?#)
	   (forward-line -1)
	   (cperl-to-comment-or-eol)
	   (setq end (point)))
      (goto-char end)
      (current-column))))


(defun cperl-to-comment-or-eol ()
  "Go to position before comment on the current line, or to end of line.
Returns true if comment is found.  In POD will not move the point."
  ;; If the line is inside other syntax groups (qq-style strings, HERE-docs)
  ;; then looks for literal # or end-of-line.
  (let (state stop-in cpoint (lim (line-end-position)) pr e)
    (or cperl-font-locking
	(cperl-update-syntaxification lim))
    (beginning-of-line)
    (if (setq pr (get-text-property (point) 'syntax-type))
	(setq e (next-single-property-change (point) 'syntax-type nil (point-max))))
    (if (or (eq pr 'pod)
	    (if (or (not e) (> e lim))	; deep inside a group
		(re-search-forward "\\=[ \t]*\\(#\\|$\\)" lim t)))
	(if (eq (preceding-char) ?\#) (progn (backward-char 1) t))
      ;; Else - need to do it the hard way
      (and (and e (<= e lim))
	   (goto-char e))
      (while (not stop-in)
	(setq state (parse-partial-sexp (point) lim nil nil nil t))
					; stop at comment
	;; If fails (beginning-of-line inside sexp), then contains not-comment
	(if (nth 4 state)		; After `#';
					; (nth 2 state) can be
					; beginning of m,s,qq and so
					; on
	    (if (nth 2 state)
		(progn
		  (setq cpoint (point))
		  (goto-char (nth 2 state))
		  (cond
		   ((looking-at "\\(s\\|tr\\)\\>")
		    (or (re-search-forward
			 "\\=\\w+[ \t]*#\\([^\n\\#]\\|\\\\[\\#]\\)*#\\([^\n\\#]\\|\\\\[\\#]\\)*"
			 lim 'move)
			(setq stop-in t)))
		   ((looking-at "\\(m\\|q\\([qxwr]\\)?\\)\\>")
		    (or (re-search-forward
			 "\\=\\w+[ \t]*#\\([^\n\\#]\\|\\\\[\\#]\\)*#"
			 lim 'move)
			(setq stop-in t)))
		   (t			; It was fair comment
		    (setq stop-in t)	; Finish
		    (goto-char (1- cpoint)))))
	      (setq stop-in t)		; Finish
	      (forward-char -1))
	  (setq stop-in t)))		; Finish
      (nth 4 state))))

(defsubst cperl-modify-syntax-type (at how)
  "Modify the syntax-table text properties at AT as given by HOW."
  (if (< at (point-max))
      (progn
	(put-text-property at (1+ at) 'syntax-table how)
	(put-text-property at (1+ at) 'rear-nonsticky '(syntax-table)))))

(defun cperl-protect-defun-start (s e)
  "Mark parentheses as punctuation between S and E.
C code looks for \"^\\s(\" to skip comment backward in \"hard\" situations."
  (save-excursion
    (goto-char s)
    (while (re-search-forward "^\\s(" e 'to-end)
      (put-text-property (1- (point)) (point) 'syntax-table cperl-st-punct))))

(defun cperl-commentify (begin end string)
  "Mark text from BEGIN to END as generic string or comment.
Mark as generic string if STRING, as generic comment otherwise.
A single character is marked as punctuation and directly
fontified.  Do nothing if BEGIN and END are equal.  If
`cperl-use-syntax-table-text-property' is nil, just fontify."
  (if (and cperl-use-syntax-table-text-property
           (> end begin))
      (progn
        (setq string (if string cperl-st-sfence cperl-st-cfence))
        (if (> begin (- end 2))
	    ;; one-char string/comment?!
	    (cperl-modify-syntax-type begin cperl-st-punct)
          (cperl-modify-syntax-type begin string)
          (cperl-modify-syntax-type (1- end) string))
        (if (and (eq string cperl-st-sfence) (> (- end 2) begin))
	    (put-text-property (1+ begin) (1- end)
			       'syntax-table cperl-string-syntax-table))
        (cperl-protect-defun-start begin end))
    ;; Fontify
    (when cperl-pod-here-fontify
      (put-text-property begin end 'face (if string 'font-lock-string-face
				           'font-lock-comment-face)))))

(defvar cperl--basic-paired-delimiters '(( ?\( . ?\) )
			                 ( ?\[ . ?\] )
			                 ( ?\{ . ?\} )
			                 ( ?\< . ?\> )))
;; -------- The following definition is generated code from  "perlop"
;; https://metacpan.org/release/HAARG/perl-5.40.0/view/pod/perlop.pod
(defvar cperl--extra-paired-delimiters '(( ?\N{U+0028} . ?\N{U+0029} )
                                         ( ?\N{U+003C} . ?\N{U+003E} )
                                         ( ?\N{U+005B} . ?\N{U+005D} )
                                         ( ?\N{U+007B} . ?\N{U+007D} )
                                         ( ?\N{U+00AB} . ?\N{U+00BB} )
                                         ( ?\N{U+00BB} . ?\N{U+00AB} )
                                         ( ?\N{U+0F3A} . ?\N{U+0F3B} )
                                         ( ?\N{U+0F3C} . ?\N{U+0F3D} )
                                         ( ?\N{U+169B} . ?\N{U+169C} )
                                         ( ?\N{U+2018} . ?\N{U+2019} )
                                         ( ?\N{U+2019} . ?\N{U+2018} )
                                         ( ?\N{U+201C} . ?\N{U+201D} )
                                         ( ?\N{U+201D} . ?\N{U+201C} )
                                         ( ?\N{U+2035} . ?\N{U+2032} )
                                         ( ?\N{U+2036} . ?\N{U+2033} )
                                         ( ?\N{U+2037} . ?\N{U+2034} )
                                         ( ?\N{U+2039} . ?\N{U+203A} )
                                         ( ?\N{U+203A} . ?\N{U+2039} )
                                         ( ?\N{U+2045} . ?\N{U+2046} )
                                         ( ?\N{U+204D} . ?\N{U+204C} )
                                         ( ?\N{U+207D} . ?\N{U+207E} )
                                         ( ?\N{U+208D} . ?\N{U+208E} )
                                         ( ?\N{U+2192} . ?\N{U+2190} )
                                         ( ?\N{U+219B} . ?\N{U+219A} )
                                         ( ?\N{U+219D} . ?\N{U+219C} )
                                         ( ?\N{U+21A0} . ?\N{U+219E} )
                                         ( ?\N{U+21A3} . ?\N{U+21A2} )
                                         ( ?\N{U+21A6} . ?\N{U+21A4} )
                                         ( ?\N{U+21AA} . ?\N{U+21A9} )
                                         ( ?\N{U+21AC} . ?\N{U+21AB} )
                                         ( ?\N{U+21B1} . ?\N{U+21B0} )
                                         ( ?\N{U+21B3} . ?\N{U+21B2} )
                                         ( ?\N{U+21C0} . ?\N{U+21BC} )
                                         ( ?\N{U+21C1} . ?\N{U+21BD} )
                                         ( ?\N{U+21C9} . ?\N{U+21C7} )
                                         ( ?\N{U+21CF} . ?\N{U+21CD} )
                                         ( ?\N{U+21D2} . ?\N{U+21D0} )
                                         ( ?\N{U+21DB} . ?\N{U+21DA} )
                                         ( ?\N{U+21DD} . ?\N{U+21DC} )
                                         ( ?\N{U+21E2} . ?\N{U+21E0} )
                                         ( ?\N{U+21E5} . ?\N{U+21E4} )
                                         ( ?\N{U+21E8} . ?\N{U+21E6} )
                                         ( ?\N{U+21F4} . ?\N{U+2B30} )
                                         ( ?\N{U+21F6} . ?\N{U+2B31} )
                                         ( ?\N{U+21F8} . ?\N{U+21F7} )
                                         ( ?\N{U+21FB} . ?\N{U+21FA} )
                                         ( ?\N{U+21FE} . ?\N{U+21FD} )
                                         ( ?\N{U+2208} . ?\N{U+220B} )
                                         ( ?\N{U+2209} . ?\N{U+220C} )
                                         ( ?\N{U+220A} . ?\N{U+220D} )
                                         ( ?\N{U+2264} . ?\N{U+2265} )
                                         ( ?\N{U+2266} . ?\N{U+2267} )
                                         ( ?\N{U+2268} . ?\N{U+2269} )
                                         ( ?\N{U+226A} . ?\N{U+226B} )
                                         ( ?\N{U+226E} . ?\N{U+226F} )
                                         ( ?\N{U+2270} . ?\N{U+2271} )
                                         ( ?\N{U+2272} . ?\N{U+2273} )
                                         ( ?\N{U+2274} . ?\N{U+2275} )
                                         ( ?\N{U+227A} . ?\N{U+227B} )
                                         ( ?\N{U+227C} . ?\N{U+227D} )
                                         ( ?\N{U+227E} . ?\N{U+227F} )
                                         ( ?\N{U+2280} . ?\N{U+2281} )
                                         ( ?\N{U+2282} . ?\N{U+2283} )
                                         ( ?\N{U+2284} . ?\N{U+2285} )
                                         ( ?\N{U+2286} . ?\N{U+2287} )
                                         ( ?\N{U+2288} . ?\N{U+2289} )
                                         ( ?\N{U+228A} . ?\N{U+228B} )
                                         ( ?\N{U+22A3} . ?\N{U+22A2} )
                                         ( ?\N{U+22A6} . ?\N{U+2ADE} )
                                         ( ?\N{U+22A8} . ?\N{U+2AE4} )
                                         ( ?\N{U+22A9} . ?\N{U+2AE3} )
                                         ( ?\N{U+22B0} . ?\N{U+22B1} )
                                         ( ?\N{U+22D0} . ?\N{U+22D1} )
                                         ( ?\N{U+22D6} . ?\N{U+22D7} )
                                         ( ?\N{U+22D8} . ?\N{U+22D9} )
                                         ( ?\N{U+22DC} . ?\N{U+22DD} )
                                         ( ?\N{U+22DE} . ?\N{U+22DF} )
                                         ( ?\N{U+22E0} . ?\N{U+22E1} )
                                         ( ?\N{U+22E6} . ?\N{U+22E7} )
                                         ( ?\N{U+22E8} . ?\N{U+22E9} )
                                         ( ?\N{U+22F2} . ?\N{U+22FA} )
                                         ( ?\N{U+22F3} . ?\N{U+22FB} )
                                         ( ?\N{U+22F4} . ?\N{U+22FC} )
                                         ( ?\N{U+22F6} . ?\N{U+22FD} )
                                         ( ?\N{U+22F7} . ?\N{U+22FE} )
                                         ( ?\N{U+2308} . ?\N{U+2309} )
                                         ( ?\N{U+230A} . ?\N{U+230B} )
                                         ( ?\N{U+2326} . ?\N{U+232B} )
                                         ( ?\N{U+2348} . ?\N{U+2347} )
                                         ( ?\N{U+23ED} . ?\N{U+23EE} )
                                         ( ?\N{U+261B} . ?\N{U+261A} )
                                         ( ?\N{U+261E} . ?\N{U+261C} )
                                         ( ?\N{U+269E} . ?\N{U+269F} )
                                         ( ?\N{U+2768} . ?\N{U+2769} )
                                         ( ?\N{U+276A} . ?\N{U+276B} )
                                         ( ?\N{U+276C} . ?\N{U+276D} )
                                         ( ?\N{U+276E} . ?\N{U+276F} )
                                         ( ?\N{U+2770} . ?\N{U+2771} )
                                         ( ?\N{U+2772} . ?\N{U+2773} )
                                         ( ?\N{U+2774} . ?\N{U+2775} )
                                         ( ?\N{U+27C3} . ?\N{U+27C4} )
                                         ( ?\N{U+27C5} . ?\N{U+27C6} )
                                         ( ?\N{U+27C8} . ?\N{U+27C9} )
                                         ( ?\N{U+27DE} . ?\N{U+27DD} )
                                         ( ?\N{U+27E6} . ?\N{U+27E7} )
                                         ( ?\N{U+27E8} . ?\N{U+27E9} )
                                         ( ?\N{U+27EA} . ?\N{U+27EB} )
                                         ( ?\N{U+27EC} . ?\N{U+27ED} )
                                         ( ?\N{U+27EE} . ?\N{U+27EF} )
                                         ( ?\N{U+27F4} . ?\N{U+2B32} )
                                         ( ?\N{U+27F6} . ?\N{U+27F5} )
                                         ( ?\N{U+27F9} . ?\N{U+27F8} )
                                         ( ?\N{U+27FC} . ?\N{U+27FB} )
                                         ( ?\N{U+27FE} . ?\N{U+27FD} )
                                         ( ?\N{U+27FF} . ?\N{U+2B33} )
                                         ( ?\N{U+2900} . ?\N{U+2B34} )
                                         ( ?\N{U+2901} . ?\N{U+2B35} )
                                         ( ?\N{U+2903} . ?\N{U+2902} )
                                         ( ?\N{U+2905} . ?\N{U+2B36} )
                                         ( ?\N{U+2907} . ?\N{U+2906} )
                                         ( ?\N{U+290D} . ?\N{U+290C} )
                                         ( ?\N{U+290F} . ?\N{U+290E} )
                                         ( ?\N{U+2910} . ?\N{U+2B37} )
                                         ( ?\N{U+2911} . ?\N{U+2B38} )
                                         ( ?\N{U+2914} . ?\N{U+2B39} )
                                         ( ?\N{U+2915} . ?\N{U+2B3A} )
                                         ( ?\N{U+2916} . ?\N{U+2B3B} )
                                         ( ?\N{U+2917} . ?\N{U+2B3C} )
                                         ( ?\N{U+2918} . ?\N{U+2B3D} )
                                         ( ?\N{U+291A} . ?\N{U+2919} )
                                         ( ?\N{U+291C} . ?\N{U+291B} )
                                         ( ?\N{U+291E} . ?\N{U+291D} )
                                         ( ?\N{U+2920} . ?\N{U+291F} )
                                         ( ?\N{U+2933} . ?\N{U+2B3F} )
                                         ( ?\N{U+2937} . ?\N{U+2936} )
                                         ( ?\N{U+2945} . ?\N{U+2946} )
                                         ( ?\N{U+2947} . ?\N{U+2B3E} )
                                         ( ?\N{U+2953} . ?\N{U+2952} )
                                         ( ?\N{U+2957} . ?\N{U+2956} )
                                         ( ?\N{U+295B} . ?\N{U+295A} )
                                         ( ?\N{U+295F} . ?\N{U+295E} )
                                         ( ?\N{U+2964} . ?\N{U+2962} )
                                         ( ?\N{U+296C} . ?\N{U+296A} )
                                         ( ?\N{U+296D} . ?\N{U+296B} )
                                         ( ?\N{U+2971} . ?\N{U+2B40} )
                                         ( ?\N{U+2972} . ?\N{U+2B41} )
                                         ( ?\N{U+2974} . ?\N{U+2B4B} )
                                         ( ?\N{U+2975} . ?\N{U+2B42} )
                                         ( ?\N{U+2979} . ?\N{U+297B} )
                                         ( ?\N{U+2983} . ?\N{U+2984} )
                                         ( ?\N{U+2985} . ?\N{U+2986} )
                                         ( ?\N{U+2987} . ?\N{U+2988} )
                                         ( ?\N{U+2989} . ?\N{U+298A} )
                                         ( ?\N{U+298B} . ?\N{U+298C} )
                                         ( ?\N{U+298D} . ?\N{U+2990} )
                                         ( ?\N{U+298F} . ?\N{U+298E} )
                                         ( ?\N{U+2991} . ?\N{U+2992} )
                                         ( ?\N{U+2993} . ?\N{U+2994} )
                                         ( ?\N{U+2995} . ?\N{U+2996} )
                                         ( ?\N{U+2997} . ?\N{U+2998} )
                                         ( ?\N{U+29A8} . ?\N{U+29A9} )
                                         ( ?\N{U+29AA} . ?\N{U+29AB} )
                                         ( ?\N{U+29B3} . ?\N{U+29B4} )
                                         ( ?\N{U+29C0} . ?\N{U+29C1} )
                                         ( ?\N{U+29D8} . ?\N{U+29D9} )
                                         ( ?\N{U+29DA} . ?\N{U+29DB} )
                                         ( ?\N{U+29FC} . ?\N{U+29FD} )
                                         ( ?\N{U+2A79} . ?\N{U+2A7A} )
                                         ( ?\N{U+2A7B} . ?\N{U+2A7C} )
                                         ( ?\N{U+2A7D} . ?\N{U+2A7E} )
                                         ( ?\N{U+2A7F} . ?\N{U+2A80} )
                                         ( ?\N{U+2A81} . ?\N{U+2A82} )
                                         ( ?\N{U+2A83} . ?\N{U+2A84} )
                                         ( ?\N{U+2A85} . ?\N{U+2A86} )
                                         ( ?\N{U+2A87} . ?\N{U+2A88} )
                                         ( ?\N{U+2A89} . ?\N{U+2A8A} )
                                         ( ?\N{U+2A8D} . ?\N{U+2A8E} )
                                         ( ?\N{U+2A95} . ?\N{U+2A96} )
                                         ( ?\N{U+2A97} . ?\N{U+2A98} )
                                         ( ?\N{U+2A99} . ?\N{U+2A9A} )
                                         ( ?\N{U+2A9B} . ?\N{U+2A9C} )
                                         ( ?\N{U+2A9D} . ?\N{U+2A9E} )
                                         ( ?\N{U+2A9F} . ?\N{U+2AA0} )
                                         ( ?\N{U+2AA1} . ?\N{U+2AA2} )
                                         ( ?\N{U+2AA6} . ?\N{U+2AA7} )
                                         ( ?\N{U+2AA8} . ?\N{U+2AA9} )
                                         ( ?\N{U+2AAA} . ?\N{U+2AAB} )
                                         ( ?\N{U+2AAC} . ?\N{U+2AAD} )
                                         ( ?\N{U+2AAF} . ?\N{U+2AB0} )
                                         ( ?\N{U+2AB1} . ?\N{U+2AB2} )
                                         ( ?\N{U+2AB3} . ?\N{U+2AB4} )
                                         ( ?\N{U+2AB5} . ?\N{U+2AB6} )
                                         ( ?\N{U+2AB7} . ?\N{U+2AB8} )
                                         ( ?\N{U+2AB9} . ?\N{U+2ABA} )
                                         ( ?\N{U+2ABB} . ?\N{U+2ABC} )
                                         ( ?\N{U+2ABD} . ?\N{U+2ABE} )
                                         ( ?\N{U+2ABF} . ?\N{U+2AC0} )
                                         ( ?\N{U+2AC1} . ?\N{U+2AC2} )
                                         ( ?\N{U+2AC3} . ?\N{U+2AC4} )
                                         ( ?\N{U+2AC5} . ?\N{U+2AC6} )
                                         ( ?\N{U+2AC7} . ?\N{U+2AC8} )
                                         ( ?\N{U+2AC9} . ?\N{U+2ACA} )
                                         ( ?\N{U+2ACB} . ?\N{U+2ACC} )
                                         ( ?\N{U+2ACF} . ?\N{U+2AD0} )
                                         ( ?\N{U+2AD1} . ?\N{U+2AD2} )
                                         ( ?\N{U+2AD5} . ?\N{U+2AD6} )
                                         ( ?\N{U+2AE5} . ?\N{U+22AB} )
                                         ( ?\N{U+2AF7} . ?\N{U+2AF8} )
                                         ( ?\N{U+2AF9} . ?\N{U+2AFA} )
                                         ( ?\N{U+2B46} . ?\N{U+2B45} )
                                         ( ?\N{U+2B47} . ?\N{U+2B49} )
                                         ( ?\N{U+2B48} . ?\N{U+2B4A} )
                                         ( ?\N{U+2B4C} . ?\N{U+2973} )
                                         ( ?\N{U+2B62} . ?\N{U+2B60} )
                                         ( ?\N{U+2B6C} . ?\N{U+2B6A} )
                                         ( ?\N{U+2B72} . ?\N{U+2B70} )
                                         ( ?\N{U+2B7C} . ?\N{U+2B7A} )
                                         ( ?\N{U+2B86} . ?\N{U+2B84} )
                                         ( ?\N{U+2B8A} . ?\N{U+2B88} )
                                         ( ?\N{U+2B95} . ?\N{U+2B05} )
                                         ( ?\N{U+2B9A} . ?\N{U+2B98} )
                                         ( ?\N{U+2B9E} . ?\N{U+2B9C} )
                                         ( ?\N{U+2BA1} . ?\N{U+2BA0} )
                                         ( ?\N{U+2BA3} . ?\N{U+2BA2} )
                                         ( ?\N{U+2BA9} . ?\N{U+2BA8} )
                                         ( ?\N{U+2BAB} . ?\N{U+2BAA} )
                                         ( ?\N{U+2BB1} . ?\N{U+2BB0} )
                                         ( ?\N{U+2BB3} . ?\N{U+2BB2} )
                                         ( ?\N{U+2BEE} . ?\N{U+2BEC} )
                                         ( ?\N{U+2E02} . ?\N{U+2E03} )
                                         ( ?\N{U+2E03} . ?\N{U+2E02} )
                                         ( ?\N{U+2E04} . ?\N{U+2E05} )
                                         ( ?\N{U+2E05} . ?\N{U+2E04} )
                                         ( ?\N{U+2E09} . ?\N{U+2E0A} )
                                         ( ?\N{U+2E0A} . ?\N{U+2E09} )
                                         ( ?\N{U+2E0C} . ?\N{U+2E0D} )
                                         ( ?\N{U+2E0D} . ?\N{U+2E0C} )
                                         ( ?\N{U+2E11} . ?\N{U+2E10} )
                                         ( ?\N{U+2E1C} . ?\N{U+2E1D} )
                                         ( ?\N{U+2E1D} . ?\N{U+2E1C} )
                                         ( ?\N{U+2E20} . ?\N{U+2E21} )
                                         ( ?\N{U+2E21} . ?\N{U+2E20} )
                                         ( ?\N{U+2E22} . ?\N{U+2E23} )
                                         ( ?\N{U+2E24} . ?\N{U+2E25} )
                                         ( ?\N{U+2E26} . ?\N{U+2E27} )
                                         ( ?\N{U+2E28} . ?\N{U+2E29} )
                                         ( ?\N{U+2E36} . ?\N{U+2E37} )
                                         ( ?\N{U+2E42} . ?\N{U+201E} )
                                         ( ?\N{U+2E55} . ?\N{U+2E56} )
                                         ( ?\N{U+2E57} . ?\N{U+2E58} )
                                         ( ?\N{U+2E59} . ?\N{U+2E5A} )
                                         ( ?\N{U+2E5B} . ?\N{U+2E5C} )
                                         ( ?\N{U+A9C1} . ?\N{U+A9C2} )
                                         ( ?\N{U+FD3E} . ?\N{U+FD3F} )
                                         ( ?\N{U+FF62} . ?\N{U+FF63} )
                                         ( ?\N{U+FFEB} . ?\N{U+FFE9} )
                                         ( ?\N{U+1D103} . ?\N{U+1D102} )
                                         ( ?\N{U+1D106} . ?\N{U+1D107} )
                                         ( ?\N{U+1F57B} . ?\N{U+1F57D} )
                                         ( ?\N{U+1F599} . ?\N{U+1F598} )
                                         ( ?\N{U+1F59B} . ?\N{U+1F59A} )
                                         ( ?\N{U+1F59D} . ?\N{U+1F59C} )
                                         ( ?\N{U+1F5E6} . ?\N{U+1F5E7} )
                                         ( ?\N{U+1F802} . ?\N{U+1F800} )
                                         ( ?\N{U+1F806} . ?\N{U+1F804} )
                                         ( ?\N{U+1F80A} . ?\N{U+1F808} )
                                         ( ?\N{U+1F812} . ?\N{U+1F810} )
                                         ( ?\N{U+1F816} . ?\N{U+1F814} )
                                         ( ?\N{U+1F81A} . ?\N{U+1F818} )
                                         ( ?\N{U+1F81E} . ?\N{U+1F81C} )
                                         ( ?\N{U+1F822} . ?\N{U+1F820} )
                                         ( ?\N{U+1F826} . ?\N{U+1F824} )
                                         ( ?\N{U+1F82A} . ?\N{U+1F828} )
                                         ( ?\N{U+1F82E} . ?\N{U+1F82C} )
                                         ( ?\N{U+1F832} . ?\N{U+1F830} )
                                         ( ?\N{U+1F836} . ?\N{U+1F834} )
                                         ( ?\N{U+1F83A} . ?\N{U+1F838} )
                                         ( ?\N{U+1F83E} . ?\N{U+1F83C} )
                                         ( ?\N{U+1F842} . ?\N{U+1F840} )
                                         ( ?\N{U+1F846} . ?\N{U+1F844} )
                                         ( ?\N{U+1F852} . ?\N{U+1F850} )
                                         ( ?\N{U+1F862} . ?\N{U+1F860} )
                                         ( ?\N{U+1F86A} . ?\N{U+1F868} )
                                         ( ?\N{U+1F872} . ?\N{U+1F870} )
                                         ( ?\N{U+1F87A} . ?\N{U+1F878} )
                                         ( ?\N{U+1F882} . ?\N{U+1F880} )
                                         ( ?\N{U+1F892} . ?\N{U+1F890} )
                                         ( ?\N{U+1F896} . ?\N{U+1F894} )
                                         ( ?\N{U+1F89A} . ?\N{U+1F898} )
                                         ( ?\N{U+1F8A1} . ?\N{U+1F8A0} )
                                         ( ?\N{U+1F8A3} . ?\N{U+1F8A2} )
                                         ( ?\N{U+1F8A5} . ?\N{U+1F8A6} )
                                         ( ?\N{U+1F8A7} . ?\N{U+1F8A4} )
                                         ( ?\N{U+1F8A9} . ?\N{U+1F8A8} )
                                         ( ?\N{U+1F8AB} . ?\N{U+1F8AA} ))
  "Full list of paired delimiters for quote-like constructs.
As an experimental feature, Perl uses these under \"feature
\\='extra_paired_delimiters\\='\" or in feature bundles of Perl 5.40 or
newer.  To activate the extra delimiters, switch on the minor mode
`cperl-extra-paired-delimiters-mode'.  This is also available from the
\"Perl\" menu in section \"Toggle...\".
The character pairs available are:
\(), <>, [], {}, «», »«, ༺༻, ༼༽, ᚛᚜, \\=‘\\=’, \\=’\\=‘, \\=“\\=”, \\=”\\=“,
\\=‵\\=′, \\=‶\\=″, \\=‷\\=‴, ‹›, ›‹, ⁅⁆,
⁍⁌, ⁽⁾, ₍₎, →←, ↛↚, ↝↜, ↠↞, ↣↢, ↦↤, ↪↩, ↬↫, ↱↰, ↳↲, ⇀↼, ⇁↽, ⇉⇇, ⇏⇍, ⇒⇐, ⇛⇚,
⇝⇜, ⇢⇠, ⇥⇤, ⇨⇦, ⇴⬰, ⇶⬱, ⇸⇷, ⇻⇺, ⇾⇽, ∈∋, ∉∌, ∊∍, ≤≥, ≦≧, ≨≩, ≪≫, ≮≯, ≰≱, ≲≳,
≴≵, ≺≻, ≼≽, ≾≿, ⊀⊁, ⊂⊃, ⊄⊅, ⊆⊇, ⊈⊉, ⊊⊋, ⊣⊢, ⊦⫞, ⊨⫤, ⊩⫣, ⊰⊱, ⋐⋑, ⋖⋗, ⋘⋙, ⋜⋝,
⋞⋟, ⋠⋡, ⋦⋧, ⋨⋩, ⋲⋺, ⋳⋻, ⋴⋼, ⋶⋽, ⋷⋾, ⌈⌉, ⌊⌋, ⌦⌫, ⍈⍇, ⏭⏮, ☛☚, ☞☜, ⚞⚟, ❨❩, ❪❫,
❬❭, ❮❯, ❰❱, ❲❳, ❴❵, ⟃⟄, ⟅⟆, ⟈⟉, ⟞⟝, ⟦⟧, ⟨⟩, ⟪⟫, ⟬⟭, ⟮⟯, ⟴⬲, ⟶⟵, ⟹⟸, ⟼⟻, ⟾⟽,
⟿⬳, ⤀⬴, ⤁⬵, ⤃⤂, ⤅⬶, ⤇⤆, ⤍⤌, ⤏⤎, ⤐⬷, ⤑⬸, ⤔⬹, ⤕⬺, ⤖⬻, ⤗⬼, ⤘⬽, ⤚⤙, ⤜⤛, ⤞⤝, ⤠⤟,
⤳⬿, ⤷⤶, ⥅⥆, ⥇⬾, ⥓⥒, ⥗⥖, ⥛⥚, ⥟⥞, ⥤⥢, ⥬⥪, ⥭⥫, ⥱⭀, ⥲⭁, ⥴⭋, ⥵⭂, ⥹⥻, ⦃⦄, ⦅⦆, ⦇⦈,
⦉⦊, ⦋⦌, ⦍⦐, ⦏⦎, ⦑⦒, ⦓⦔, ⦕⦖, ⦗⦘, ⦨⦩, ⦪⦫, ⦳⦴, ⧀⧁, ⧘⧙, ⧚⧛, ⧼⧽, ⩹⩺, ⩻⩼, ⩽⩾, ⩿⪀,
⪁⪂, ⪃⪄, ⪅⪆, ⪇⪈, ⪉⪊, ⪍⪎, ⪕⪖, ⪗⪘, ⪙⪚, ⪛⪜, ⪝⪞, ⪟⪠, ⪡⪢, ⪦⪧, ⪨⪩, ⪪⪫, ⪬⪭, ⪯⪰, ⪱⪲,
⪳⪴, ⪵⪶, ⪷⪸, ⪹⪺, ⪻⪼, ⪽⪾, ⪿⫀, ⫁⫂, ⫃⫄, ⫅⫆, ⫇⫈, ⫉⫊, ⫋⫌, ⫏⫐, ⫑⫒, ⫕⫖, ⫥⊫, ⫷⫸, ⫹⫺,
⭆⭅, ⭇⭉, ⭈⭊, ⭌⥳, ⭢⭠, ⭬⭪, ⭲⭰, ⭼⭺, ⮆⮄, ⮊⮈, ⮕⬅, ⮚⮘, ⮞⮜, ⮡⮠, ⮣⮢, ⮩⮨, ⮫⮪, ⮱⮰, ⮳⮲,
⯮⯬, ⸂⸃, ⸃⸂, ⸄⸅, ⸅⸄, ⸉⸊, ⸊⸉, ⸌⸍, ⸍⸌, ⸑⸐, ⸜⸝, ⸝⸜, ⸠⸡, ⸡⸠, ⸢⸣, ⸤⸥, ⸦⸧, ⸨⸩, ⸶⸷,
⹂„, ⹕⹖, ⹗⹘, ⹙⹚, ⹛⹜, ꧁꧂, ﴾﴿, ｢｣, ￫￩, 𝄃𝄂, 𝄆𝄇, 🕻🕽, 🖙🖘, 🖛🖚, 🖝🖜, 🗦🗧, 🠂🠀, 🠆🠄, 🠊🠈,
🠒🠐, 🠖🠔, 🠚🠘, 🠞🠜, 🠢🠠, 🠦🠤, 🠪🠨, 🠮🠬, 🠲🠰, 🠶🠴, 🠺🠸, 🠾🠼, 🡂🡀, 🡆🡄, 🡒🡐, 🡢🡠, 🡪🡨, 🡲🡰, 🡺🡸,
🢂🢀, 🢒🢐, 🢖🢔, 🢚🢘, 🢡🢠, 🢣🢢, 🢥🢦, 🢧🢤, 🢩🢨, 🢫🢪")

;; --------End of generated code
(defvar cperl-starters cperl--basic-paired-delimiters)

(defun cperl-cached-syntax-table (st)
  "Get a syntax table cached in ST, or create and cache into ST a syntax table.
All the entries of the syntax table are \".\", except for a backslash, which
is quoting."
  (if (car-safe st)
      (car st)
    (setcar st (make-syntax-table))
    (setq st (car st))
    (let ((i 0))
      (while (< i 256)
	(modify-syntax-entry i "." st)
	(setq i (1+ i))))
    (modify-syntax-entry ?\\ "\\" st)
    st))

(defun cperl-forward-re (lim end is-2arg st-l err-l argument
			     &optional ostart oend)
"Find the end of a regular expression or a stringish construct (q[] etc).
The point should be before the starting delimiter.

Goes to LIM if none is found.  If IS-2ARG is non-nil, assumes that it
is s/// or tr/// like expression.  If END is nil, generates an error
message if needed.  If SET-ST is non-nil, will use (or generate) a
cached syntax table in ST-L.  If ERR-L is non-nil, will store the
error message in its CAR (unless it already contains some error
message).  ARGUMENT should be the name of the construct (used in error
messages).  OSTART, OEND may be set in recursive calls when processing
the second argument of 2ARG construct.

Works *before* syntax recognition is done.  In IS-2ARG situation may
modify syntax-type text property if the situation is too hard."
  (let (b starter ender st i i2 go-forward reset-st set-st)
    (skip-chars-forward " \t")
    ;; ender means matching-char matcher.
    (setq b (point)
	  starter (if (eobp) 0 (char-after b))
	  ender (cdr (assoc starter cperl-starters)))
    ;; What if starter == ?\\  ????
    (setq st (cperl-cached-syntax-table st-l))
    (setq set-st t)
    ;; Whether we have an intermediate point
    (setq i nil)
    ;; Prepare the syntax table:
    (if (not ender)		; m/blah/, s/x//, s/x/y/
	(modify-syntax-entry starter "$" st)
      (modify-syntax-entry starter (concat "(" (list ender)) st)
      (modify-syntax-entry ender  (concat ")" (list starter)) st))
    (condition-case bb
	(progn
	  ;; We use `$' syntax class to find matching stuff, but $$
	  ;; is recognized the same as $, so we need to check this manually.
	  (if (and (eq starter (char-after (cperl-1+ b)))
		   (not ender))
	      ;; $ has TeXish matching rules, so $$ equiv $...
	      (forward-char 2)
	    (setq reset-st (syntax-table))
	    (set-syntax-table st)
	    (forward-sexp 1)
	    (if (<= (point) (1+ b))
		(error "Unfinished regular expression"))
	    (set-syntax-table reset-st)
	    (setq reset-st nil)
	    ;; Now the problem is with m;blah;;
	    (and (not ender)
		 (eq (preceding-char)
		     (char-after (- (point) 2)))
		 (save-excursion
		   (forward-char -2)
		   (cl-evenp (skip-chars-backward "\\\\")))
		 (forward-char -1)))
	  ;; Now we are after the first part.
	  (and is-2arg			; Have trailing part
	       (not ender)
	       (eq (following-char) starter) ; Empty trailing part
	       (progn
		 (or (eq (char-syntax (following-char)) ?.)
		     ;; Make trailing letter into punctuation
		     (cperl-modify-syntax-type (point) cperl-st-punct))
		 (setq is-2arg nil go-forward t))) ; Ignore the tail
	  (if is-2arg			; Not number => have second part
	      (progn
		(setq i (point) i2 i)
		(if ender
		    (if (memq (following-char) '(?\s ?\t ?\n ?\f))
			(progn
			  (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
			      (goto-char (match-end 0))
			    (skip-chars-forward " \t\n\f"))
			  (setq i2 (point))))
		  (forward-char -1))
		(modify-syntax-entry starter (if (eq starter ?\\) "\\" ".") st)
		(if ender (modify-syntax-entry ender "." st))
		(setq set-st nil)
		(setq ender (cperl-forward-re lim end nil st-l err-l
					      argument starter ender)
		      ender (nth 2 ender)))))
      (error (goto-char lim)
	     (setq set-st nil)
	     (if reset-st
		 (set-syntax-table reset-st))
	     (or end
		 (and cperl-brace-recursing
		      (or (eq ostart  ?\{)
			  (eq starter ?\{)))
		 (message
		  "End of `%s%s%c ... %c' string/RE not found: %s"
		  argument
		  (if ostart (format "%c ... %c" ostart (or oend ostart)) "")
		  starter (or ender starter) bb)
		 (or (car err-l) (setcar err-l b)))))
    (if set-st
	(progn
	  (modify-syntax-entry starter (if (eq starter ?\\) "\\" ".") st)
	  (if ender (modify-syntax-entry ender "." st))))
    ;; i: have 2 args, after end of the first arg
    ;; i2: start of the second arg, if any (before delim if `ender').
    ;; ender: the last arg bounded by parens-like chars, the second one of them
    ;; starter: the starting delimiter of the first arg
    ;; go-forward: has 2 args, and the second part is empty
    (list i i2 ender starter go-forward)))

(defun cperl-forward-group-in-re (&optional st-l)
  "Find the end of a group in a REx.
Return the error message (if any).  Does not work if delimiter is `)'.
Works before syntax recognition is done.
ST-L is a cached syntax table to use."
  ;; Works *before* syntax recognition is done
  (or st-l (setq st-l (list nil)))	; Avoid overwriting '()
  (let (st result reset-st)
    (condition-case err
	(progn
	  (setq st (cperl-cached-syntax-table st-l))
	  (modify-syntax-entry ?\( "()" st)
	  (modify-syntax-entry ?\) ")(" st)
	  (setq reset-st (syntax-table))
	  (set-syntax-table st)
	  (forward-sexp 1))
      (error (setq result err)))
    ;; now restore the initial state
    (if st
	(progn
	  (modify-syntax-entry ?\( "." st)
	  (modify-syntax-entry ?\) "." st)))
    (if reset-st
	(set-syntax-table reset-st))
    result))


(defsubst cperl-postpone-fontification (b e type val &optional now)
  "Prepare text between B and E for postponed fontification.
TYPE is now always the symbol `face', VAL is the value (the actual face)
to be applied.  This works by adding a text property `cperl-postpone' to
the range, which later is evaluated in the function
`cperl-fontify-update' which is inserted into the `font-lock-defaults'.
If `cperl-syntaxify-by-font-lock' is nil (which is not recommended),
then the text property TYPE is immediately set to VAL.
NOW is no longer useful."
  (if cperl-syntaxify-by-font-lock
      (or now (put-text-property b e 'cperl-postpone (cons type val)))
    (put-text-property b e type val)))

;; Here is how the global structures (those which cannot be
;; recognized locally) are marked:
;;	a) PODs:
;;		Start-to-end is marked `in-pod' ==> t
;;		Each non-literal part is marked `syntax-type' ==> `pod'
;;		Each literal part is marked `syntax-type' ==> `in-pod'
;;	b) HEREs:
;;              The point before start is marked `here-doc-start'
;;		Start-to-end is marked `here-doc-group' ==> t
;;		The body is marked `syntax-type' ==> `here-doc'
;;                and is also marked as style 2 comment
;;		The delimiter is marked `syntax-type' ==> `here-doc-delim'
;;	c) FORMATs:
;;		First line (to =) marked `first-format-line' ==> t
;;		After-this--to-end is marked `syntax-type' ==> `format'
;;	d) 'Q'uoted string:
;;		part between markers inclusive is marked `syntax-type' ==> `string'
;;		part between `q' and the first marker is marked `syntax-type' ==> `prestring'
;;		second part of s///e is marked `syntax-type' ==> `multiline'
;;	e) Attributes of subroutines: `attrib-group' ==> t
;;		(or 0 if declaration); up to `{' or ';': `syntax-type' => `sub-decl'.
;;      f) Multiline my/our declaration lists etc: `syntax-type' => `multiline'

;; In addition, some parts of RExes may be marked as `REx-interpolated'
;; (value: 0 in //o, 1 if "interpolated variable" is whole-REx, t otherwise).

(defun cperl-unwind-to-safe (before &optional end)
  "Move point back to a safe place, back up one extra line if BEFORE.
A place is \"safe\" if it is not within POD, a here-document, a
format, a quote-like expression, a subroutine attribute list or a
multiline declaration.  These places all have special syntactical
rules and need to be parsed as a whole.  If END, return the
position of the end of the unsafe construct."
  (let ((pos (point))
        (state (syntax-ppss)))
    ;; Check edge cases for here-documents first
    (when before                        ; we need a safe start for parsing
      (cond
       ((or (equal (get-text-property (cperl-1- (point)) 'syntax-type)
                   'here-doc-start)
            (equal (syntax-after (cperl-1- (point)))
                   (string-to-syntax "> c")))
        ;; point is either immediately after the start of a here-doc
        ;; (which may consist of nothing but one newline) or
        ;; immediately after the now-outdated end marker of the
        ;; here-doc. In both cases we need to back up to the line
        ;; where the here-doc delimiters are defined.
        (forward-char -1)
        (cperl-backward-to-noncomment (point-min))
        (beginning-of-line))
       ((eq 2 (nth 7 state))
        ;; point is somewhere in a here-document.  Back up to the line
        ;; where the here-doc delimiters are defined.
        (goto-char (nth 8 state))      ; beginning of this here-doc
        (cperl-backward-to-noncomment  ; skip back over more
         (point-min))                  ;     here-documents (if any)
        (beginning-of-line))           ; skip back over here-doc starters
       ((nth 4 state)                  ; in a comment (or POD)
        (goto-char (nth 8 state)))))   ; ...so go to its beginning
    (while (and pos (progn
		      (beginning-of-line)
		      (get-text-property (setq pos (point)) 'syntax-type)))
      (setq pos (cperl-beginning-of-property pos 'syntax-type))
      (if (eq pos (point-min))
	  (setq pos nil))
      (if pos
	  (if before
	      (progn
		(goto-char (cperl-1- pos))
		(beginning-of-line)
		(setq pos (point)))
	    (goto-char (setq pos (cperl-1- pos))))
	;; Up to the start
	(goto-char (point-min))))
    ;; Skip empty lines
    (and (looking-at "\n*=")
	 (/= 0 (skip-chars-backward "\n"))
	 (forward-char))
    (setq pos (point))
    (if end
	;; Do the same for end, going small steps
	(save-excursion
	  (while (and end (< end (point-max))
		      (get-text-property end 'syntax-type))
	    (setq pos end
		  end (next-single-property-change end 'syntax-type nil (point-max)))
	    (if end (progn (goto-char end)
			   (or (bolp) (forward-line 1))
			   (setq end (point)))))
	  (or end pos)))))

(defun cperl-find-sub-attrs (&optional st-l _b-fname _e-fname pos)
  "Syntactically mark (and fontify) attributes of a subroutine.
Within attributes, parentheses and text between them have weird
syntactical properties which are likely to mess up search-based
fontification.  Therefore they are fontified explicitly here.
Should be called with the point before leading colon of
an attribute.  ST-L and POS are a cached from a previous call."
  ;; Works *before* syntax recognition is done
  (or st-l (setq st-l (list nil)))	; Avoid overwriting '()
  (let (st p reset-st after-first (start (point)) start1 end1)
    (condition-case b
	(while (looking-at
		(concat
		 "\\("			; 1=optional? colon
		   ":" cperl-maybe-white-and-comment-rex ; 2=whitespace/comment?
		 "\\)"
		 (if after-first "?" "")
		 ;; No space between name and paren allowed...
		 (rx (group (eval cperl--basic-identifier-rx)))	; 3=name
		 "\\((\\)?"))		; 4=optional paren
	  (and (match-beginning 1)
	       (cperl-postpone-fontification
		(match-beginning 0) (cperl-1+ (match-beginning 0))
                'face 'font-lock-constant-face))
	  (setq start1 (match-beginning 3) end1 (match-end 3))
	  (cperl-postpone-fontification start1 end1
                                        'face 'font-lock-constant-face)
	  (goto-char end1)		; end or before `('
	  (if (match-end 4)		; Have attribute arguments...
	      (progn
		(if st nil
		  (setq st (cperl-cached-syntax-table st-l))
		  (modify-syntax-entry ?\( "()" st)
		  (modify-syntax-entry ?\) ")(" st))
		(setq reset-st (syntax-table) p (point))
		(set-syntax-table st)
		(forward-sexp 1)
		(set-syntax-table reset-st)
		(setq reset-st nil)
		(cperl-commentify p (point) t))) ; mark as string
	  (forward-comment (buffer-size))
	  (setq after-first t))
      (error (message
	      "L%d: attribute `%s': %s"
	      (count-lines (point-min) (point))
	      (and start1 end1 (buffer-substring start1 end1)) b)
	     ; (setq start nil) I'd like to keep trying -- haj 2023-06-26
             ))
    (cond
     ;; Allow for a complete signature and trailing spaces here
     ((search-forward-regexp (rx (sequence point
                                           (eval cperl--ws*-rx)
                                           (eval cperl--signature-rx)
                                           (eval cperl--ws*-rx)))
                             nil
                             t)) ; NOERROR
     ((looking-at (rx "("))
      ;; We might be in the process of typing a prototype or
      ;; signature.  These start with a left paren, so we want this to
      ;; be included into the area marked as sub-decl.
      nil)
     ;; Else, we are in no mans land.  Just keep trying.
     (t
      ))
    (when (looking-at (rx (in ";{")))
      ;; A semicolon ends the declaration, an opening brace begins the
      ;; BLOCK.  Neither is part of the declaration.
      (backward-char))
    (when start
      (put-text-property start (point)
                         'attrib-group (if (looking-at "{") t 0))
        (and pos
             (progn
               ;; Apparently, we do not need `multiline': faces added now
               (put-text-property (+ 3 pos) (cperl-1+ (point))
		                  'syntax-type 'sub-decl))))
    ;; now restore the initial state
    (if st
	(progn
	  (modify-syntax-entry ?\( "." st)
	  (modify-syntax-entry ?\) "." st)))
    (if reset-st
	(set-syntax-table reset-st))))

(defsubst cperl-look-at-leading-count (is-x-REx e)
  "Mark suspicious regexp content up to E.
If IS-X-REX is non-nil, then process a regular expression which has an
/x modifier."
  (if (and
       (< (point) e)
       (re-search-forward (concat "\\=" (if is-x-REx "[ \t\n]*" "") "[{?+*]")
			  (1- e) t))	; return nil on failure, no moving
      (if (eq ?\{ (preceding-char)) nil
	(cperl-postpone-fontification
	 (1- (point)) (point)
         'face 'font-lock-warning-face))))

;; Do some smarter-highlighting
;; XXXX Currently ignores alphanum/dash delims,
(defsubst cperl-highlight-charclass (endbracket dashface bsface onec-space)
  "Process the special syntax of character classes for fontification.
ENDBRACKET is the position of the closing bracket, DASHFACE, BSFACE and
ONEC-SPACE are the faces to be applied to a range indicator, characters
and character escapes, respectively."
  (let ((l '(1 5 7)) ll lle lll
	;; 2 groups, the first takes the whole match (include \[trnfabe])
	(singleChar (concat "\\(" "[^\\]" "\\|" "\\\\[^cdg-mo-qsu-zA-Z0-9_]" "\\|" "\\\\c." "\\|" "\\\\x" "\\([[:xdigit:]][[:xdigit:]]?\\|\\={[[:xdigit:]]+}\\)" "\\|" "\\\\0?[0-7][0-7]?[0-7]?" "\\|" "\\\\N{[^{}]*}" "\\)")))
    (while				; look for unescaped - between non-classes
	(re-search-forward
	 ;; On 19.33, certain simplifications lead
	 ;; to bugs (as in  [^a-z] \\| [trnfabe]  )
	 (concat	       		; 1: SingleChar (include \[trnfabe])
	  singleChar
	  ;;"\\(" "[^\\]" "\\|" "\\\\[^cdg-mo-qsu-zA-Z0-9_]" "\\|" "\\\\c." "\\|" "\\\\x" "\\([[:xdigit:]][[:xdigit:]]?\\|\\={[[:xdigit:]]+}\\)" "\\|" "\\\\0?[0-7][0-7]?[0-7]?" "\\|" "\\\\N{[^{}]*}" "\\)"
	  "\\("				; 3: DASH SingleChar (match optionally)
	    "\\(-\\)"			; 4: DASH
	    singleChar			; 5: SingleChar
	    ;;"\\(" "[^\\]" "\\|" "\\\\[^cdg-mo-qsu-zA-Z0-9_]" "\\|" "\\\\c." "\\|" "\\\\x" "\\([[:xdigit:]][[:xdigit:]]?\\|\\={[[:xdigit:]]+}\\)" "\\|" "\\\\0?[0-7][0-7]?[0-7]?" "\\|" "\\\\N{[^{}]*}" "\\)"
	  "\\)?"
	  "\\|"
	  "\\("				; 7: other escapes
	    "\\\\[pP]" "\\([^{]\\|{[^{}]*}\\)"
	    "\\|" "\\\\[^pP]" "\\)"
	  )
	 endbracket 'toend)
      (if (match-beginning 4)
	  (cperl-postpone-fontification
	   (match-beginning 4) (match-end 4)
	   'face dashface))
      ;; save match data (for looking-at)
      (setq lll (mapcar (lambda (elt) (cons (match-beginning elt)
                                       (match-end elt)))
                        l))
      (while lll
	(setq ll (car lll))
	(setq lle (cdr ll)
	      ll (car ll))
	;; (message "Got %s of %s" ll l)
	(if (and ll (eq (char-after ll) ?\\ ))
	    (save-excursion
	      (goto-char ll)
	      (cperl-postpone-fontification ll (1+ ll)
	       'face bsface)
	      (if (looking-at "\\\\[a-zA-Z0-9]")
		  (cperl-postpone-fontification (1+ ll) lle
		   'face onec-space))))
	(setq lll (cdr lll))))
    (goto-char endbracket)		; just in case something misbehaves???
    t))

(defvar cperl-here-doc-functions
  (regexp-opt '("print" "printf" "say"  ; print $handle <<EOF
                "system" "exec"         ; system $progname <<EOF
                "sort")                 ; sort $subname <<EOF
              'symbols)                 ; avoid false positives
  "List of keywords after which `$var <<bareword' is a here-document.
After any other token `$var <<bareword' is treated as the variable `$var'
left-shifted by the return value of the function `bareword'.")

(defun cperl-is-here-doc-p (start)
  "Find out whether a \"<<\" construct starting at START is a here-document.
The point is expected to be after the end of the delimiter.
Quoted delimiters after \"<<\" are unambiguously starting
here-documents and are not handled here.  This function does not
move point but does change match data."
  ;; not a here-doc | here-doc
  ;; $foo << b;     | $f .= <<B;
  ;; ($f+1) << b;   | a($f) . <<B;
  ;; foo 1, <<B;    | $x{a} <<b;
  ;; Limitations:
  ;; foo <<bar is statically undecidable.  It could be either
  ;; foo() << bar # left shifting the return value or
  ;; foo(<<bar)   # passing a here-doc to foo().
  ;; We treat it as here-document and kindly ask programmers to
  ;; disambiguate by adding parens.
  (null
   (or (looking-at "[ \t]*(") ; << function_call()
       (looking-at ">>")      ; <<>> operator
       (save-excursion ; 1 << func_name, or $foo << 10
	 (condition-case nil
	     (progn
	       (goto-char start)
	       (forward-sexp -1) ;; examine the part before "<<"
	       (save-match-data
		 (cond
		  ((looking-at "[0-9$({]")
		   (forward-sexp 1)
		   (and
		    (looking-at "[ \t]*<<")
		    (condition-case nil
			;; print $foo <<EOF
			(progn
			  (forward-sexp -2)
			  (not
			   (looking-at cperl-here-doc-functions)))
		      (error t)))))))
	   (error nil)))))) ; func(<<EOF)

(defun cperl-process-here-doc (min max end overshoot stop-point
                                   end-of-here-doc err-l
                                   indented-here-doc-p
                                   matched-pos todo-pos
                                   delim-begin delim-end)
  "Process a here-document's delimiters and body.
The parameters MIN, MAX, END, OVERSHOOT, STOP-POINT, ERR-L are
used for recursive calls to `cperl-find-pods-heres' to handle the
rest of the line which contains the delimiter.  MATCHED-POS and
TODO-POS are initial values for this function's result.
END-OF-HERE-DOC is the end of a previous here-doc in the same
line, or nil if this is the first.  DELIM-BEGIN and DELIM-END are
the positions where the here-document's delimiter has been found.
This is part of `cperl-find-pods-heres' (below)."
  (let* ((my-cperl-delimiters-face 'font-lock-constant-face)
         (delimiter (buffer-substring-no-properties delim-begin delim-end))
         (qtag (regexp-quote delimiter))
         (use-syntax-state (and cperl-syntax-state
			        (>= min (car cperl-syntax-state))))
         (state-point (if use-syntax-state
			  (car cperl-syntax-state)
		        (point-min)))
         (state (if use-syntax-state
		    (cdr cperl-syntax-state)))
         here-doc-start here-doc-end defs-eol
         warning-message)
    (when cperl-pod-here-fontify
      ;; Highlight the starting delimiter
      (cperl-postpone-fontification delim-begin delim-end
                                    'face my-cperl-delimiters-face)
      (cperl-put-do-not-fontify delim-begin delim-end t))
    (forward-line)
    (setq here-doc-start (point) ; first char of (first) here-doc
          defs-eol (1- here-doc-start)) ; end of definitions line
    (if end-of-here-doc
        ;; skip to the end of the previous here-doc
	(goto-char end-of-here-doc)
      ;; otherwise treat the first (or only) here-doc: Check for
      ;; special cases if the line containing the delimiter(s)
      ;; ends in a regular comment or a solitary ?#
      (let* ((eol-state (save-excursion (syntax-ppss defs-eol))))
        (when (nth 4 eol-state) ; EOL is in a comment
          (if (= (1- defs-eol) (nth 8 eol-state))
              ;; line ends with a naked comment starter.
              ;; We let it start the here-doc.
              (progn
                (put-text-property (1- defs-eol) defs-eol
                                   'font-lock-face
                                   'font-lock-comment-face)
                (put-text-property (1- defs-eol) defs-eol
                                   'syntax-type 'here-doc)
                (put-text-property (1- defs-eol) defs-eol
                                   'syntax-type 'here-doc)
                (put-text-property (1- defs-eol) defs-eol
                                   'syntax-table
                                   (string-to-syntax "< c"))
                )
            ;; line ends with a "regular" comment: make
            ;; the last character of the comment closing
            ;; it so that we can use the line feed to
            ;; start the here-doc
            (put-text-property (1- defs-eol) defs-eol
                               'syntax-table
                               (string-to-syntax ">"))))))
    (setq here-doc-start (point)) ; now points to current here-doc
    ;; Find the terminating delimiter.
    ;; We do not search to max, since we may be called from
    ;; some hook of fontification, and max is random
    (or (re-search-forward
	 (concat "^" (when indented-here-doc-p "[ \t]*")
		 qtag "$")
	 stop-point 'toend)
	(progn		; Pretend we matched at the end
	  (goto-char (point-max))
	  (re-search-forward "\\'")
	  (setq warning-message
                (format "End of here-document `%s' not found." delimiter))
	  (or (car err-l) (setcar err-l here-doc-start))))
    (when cperl-pod-here-fontify
      ;; Highlight the ending delimiter
      (cperl-postpone-fontification
       (match-beginning 0) (match-end 0)
       'face my-cperl-delimiters-face)
      (cperl-put-do-not-fontify here-doc-start (match-end 0) t))
    (setq here-doc-end (cperl-1+ (match-end 0))) ; eol after delim
    (put-text-property here-doc-start (match-beginning 0)
		       'syntax-type 'here-doc)
    (put-text-property (match-beginning 0) here-doc-end
		       'syntax-type 'here-doc-delim)
    (put-text-property here-doc-start here-doc-end 'here-doc-group t)
    ;; This makes insertion at the start of HERE-DOC update
    ;; the whole construct:
    (put-text-property here-doc-start (cperl-1+ here-doc-start) 'front-sticky '(syntax-type))
    (cperl-commentify (match-beginning 0) (1- here-doc-end) nil)
    (put-text-property (1- here-doc-start) here-doc-start
                       'syntax-type 'here-doc-start)
    (when (> (match-beginning 0) here-doc-start)
      ;; here-document has non-zero length
      (cperl-modify-syntax-type (1- here-doc-start) (string-to-syntax "< c"))
      (cperl-modify-syntax-type (1- (match-beginning 0))
                                (string-to-syntax "> c")))
    (cperl-put-do-not-fontify here-doc-start (match-end 0) t)
    ;; Cache the syntax info...
    (setq cperl-syntax-state (cons state-point state))
    ;; ... and process the rest of the line...
    (setq overshoot
	  (elt		; non-inter ignore-max
	   (cperl-find-pods-heres todo-pos defs-eol
                                  t end t here-doc-end)
           1))
    (if (and overshoot (> overshoot (point)))
	(goto-char overshoot)
      (setq overshoot here-doc-end))
    (list (if (> here-doc-end max) matched-pos nil)
          overshoot
          warning-message)))

(defun cperl-find-pods-heres (&optional min max non-inter end ignore-max end-of-here-doc)
  "Scan the buffer for hard-to-parse Perl constructions.
If `cperl-pod-here-fontify' is non-nil after evaluation,
fontify the sections using `cperl-pod-head-face',
`cperl-pod-face', `cperl-here-face'.  The optional parameters are
for internal use: scan from MIN to MAX, or the whole buffer if
these are nil.  If NON-INTER, don't write progress messages.  If
IGNORE-MAX, scan to end of buffer.  If END, we are after a
\"__END__\" or \"__DATA__\" token, so ignore unbalanced
constructs.  END-OF-HERE-DOC points to the end of a here-document
which has already been processed.
Value is a two-element list of the position where an error
occurred (if any) and the \"overshoot\", which is used for
recursive calls in starting lines of here-documents."
  (interactive)
  (or min (setq min (point-min)
		cperl-syntax-state nil
		cperl-syntax-done-to min))
  (or max (setq max (point-max)))
  (font-lock-flush min max)
  (let* (go tmpend
	 face head-face b e bb tag qtag b1 e1 argument i c tail tb
	 is-REx is-x-REx REx-subgr-start REx-subgr-end was-subgr i2 hairy-RE
	 (case-fold-search nil) (inhibit-read-only t) (buffer-undo-list t)
	 (modified (buffer-modified-p)) overshoot is-o-REx name
	 (inhibit-modification-hooks t)
	 (cperl-font-locking t)
	 (use-syntax-state (and cperl-syntax-state
				(>= min (car cperl-syntax-state))))
	 (state-point (if use-syntax-state
			  (car cperl-syntax-state)
			(point-min)))
	 (state (if use-syntax-state
		    (cdr cperl-syntax-state)))
	 ;; (st-l '(nil)) (err-l '(nil)) ; Would overwrite - propagates from a function call to a function call!
	 (st-l (list nil)) (err-l (list nil))
         (my-cperl-delimiters-face
          'font-lock-constant-face)
	 (my-cperl-REx-spec-char-face	; [] ^.$ and wrapper-of ({})
          'font-lock-function-name-face)
	 (my-cperl-REx-0length-face ; 0-length, (?:)etc, non-literal \
          'font-lock-builtin-face)
	 (my-cperl-REx-ctl-face		; (|)
          'font-lock-keyword-face)
	 (my-cperl-REx-modifiers-face	; //gims
	  'cperl-nonoverridable-face)
	 (my-cperl-REx-length1-face	; length=1 escaped chars, POSIX classes
          'font-lock-type-face)
	 (stop-point (if ignore-max
			 (point-max)
		       max))
	 (search
	  (concat
           ;; -------- POD using capture group 1
	   "\\(\\`\n?\\|^\n\\)="
	   "\\|"
           ;; -------- HERE-document capture groups 2-7
	   ;; One extra () before this:
	   "<<\\(~?\\)"		 ; HERE-DOC, indented-p = capture 2
	   "\\("			; 2 + 1
	   ;; First variant "BLAH" or just ``.
	   "[ \t]*"			; Yes, whitespace is allowed!
	   "\\([\"'`]\\)"		; 3 + 1 = 4
	   "\\([^\"'`\n]*\\)"		; 4 + 1
	   "\\4"
	   "\\|"
	   ;; Second variant: Identifier or \ID (same as 'ID')
	   "\\\\?\\(\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\)" ; 5 + 1, 6 + 1
	   "\\)"
	   "\\|"
           ;; -------- format capture groups 8-9
	   ;; 1+6 extra () before this:
	   "^[ \t]*\\(format\\)[ \t]*\\([a-zA-Z0-9_]+\\)?[ \t]*=[ \t]*$"
	   (if cperl-use-syntax-table-text-property
	       (concat
		"\\|"
                ;; -------- quoted constructs and regexps, group 10
		;; 1+6+2=9 extra () before this:
		"\\<\\(q[wxqr]?\\|[msy]\\|tr\\)\\>"
		"\\|"
                ;; -------- "bare" regex or glob, group 11
		;; 1+6+2+1=10 extra () before this:
		"\\([/<]\\)"	; /blah/ or <file*glob>
		"\\|"
                ;; -------- subroutine declarations, groups 12-17
		;; 1+6+2+1+1=11 extra () before this
		(rx (sequence
                     word-start
                     (group (regexp cperl-sub-regexp))                ; #12
                     (eval cperl--ws+-rx)
                     (opt (group (eval cperl--normal-identifier-rx))) ; #13
                     (eval cperl--ws*-rx)
                     (group (or (group (eval cperl--prototype-rx))    ; #14,#15
                                (group (eval cperl--signature-rx))    ; #16
                                ;; (group unmatchable) ; #16
                                (group (or anything buffer-end)))))) ; #17
		"\\|"
                ;; -------- weird variables, capture group 18
                ;; FIXME: We don't need that group -- haj 2023-06-21
                ;; 1+6+2+1+1+6=17 extra () before this
		"\\$\\(['{]\\)"		; $' or ${foo}
		"\\|"
                ;; -------- old-style ' as package separator, group 19
		;; 1+6+2+1+1+6+1=18 extra () before this (old pack'var syntax;
		;; we do not support intervening comments...):
		"\\(\\<" cperl-sub-regexp "[ \t\n\f]+\\|[&*$@%]\\)[a-zA-Z0-9_]*'"
		"\\|"
                ;; -------- __END__ and __DATA__ tokens, group 20
		;; 1+6+2+1+1+6+1+1=19 extra () before this:
		"__\\(END\\|DATA\\)__"	; __END__ or __DATA__
		;; 1+6+2+1+1+6+1+1+1=20 extra () before this:
		"\\|"
                ;; -------- backslash-escaped stuff, don't interpret it
		"\\\\\\(['`\"($]\\)"	; BACKWACKED something-hairy
                "\\|"
                ;; -------- $\ is a variable in code, but not in a string
                "\\(\\$\\\\\\)")
	     "")))
         warning-message)
    (unwind-protect
	(progn
	  (save-excursion
	    (or non-inter
		(message "Scanning for \"hard\" Perl constructions..."))
	    ;;(message "find: %s --> %s" min max)
	    (and cperl-pod-here-fontify
		 ;; We had evals here, do not know why...
		 (setq face cperl-pod-face
		       head-face cperl-pod-head-face))
            (unless end-of-here-doc
	      (remove-text-properties min max
				      '(syntax-type t in-pod t syntax-table t
						    attrib-group t
						    REx-interpolated t
						    cperl-postpone t
						    syntax-subtype t
						    rear-nonsticky t
						    front-sticky t
						    here-doc-group t
						    first-format-line t
						    REx-part2 t
						    indentable t)))
	    ;; Need to remove face as well...
	    (goto-char min)
	    (while (and
		    (< (point) max)
		    (re-search-forward search max t))
	      (setq tmpend nil)		; Valid for most cases
	      (setq b (match-beginning 0)
		    state (save-excursion (parse-partial-sexp
					   state-point b nil nil state))
		    state-point b)
	      (cond
	       ;; 1+6+2+1+1+6=17 extra () before this:
	       ;;    "\\$\\(['{]\\)"
	       ((match-beginning 18) ; $' or ${foo}
		(if (eq (preceding-char) ?\') ; $'
		    (progn
		      (setq b (1- (point))
			    state (parse-partial-sexp
				   state-point (1- b) nil nil state)
			    state-point (1- b))
		      (if (nth 3 state)	; in string
			  (cperl-modify-syntax-type (1- b) cperl-st-punct))
		      (goto-char (1+ b)))
		  ;; else: ${
		  (setq bb (match-beginning 0))
		  (cperl-modify-syntax-type bb cperl-st-punct)))
	       ;; No processing in strings/comments beyond this point:
	       ((or (nth 3 state) (nth 4 state))
                ;; Edge case: In a double-quoted string, $\ is not the
                ;; punctuation variable, $ must not quote \ here.  We
                ;; generally make $ a punctuation character in strings
                ;; and comments (Bug#69604).
                (when (match-beginning 22)
                  (cperl-modify-syntax-type (match-beginning 22) cperl-st-punct)))
	       ((match-beginning 1)	; POD section
		;;  "\\(\\`\n?\\|^\n\\)="
		(setq b (match-beginning 0)
		      state (parse-partial-sexp
			     state-point b nil nil state)
		      state-point b)
		(if (or (nth 3 state) (nth 4 state)
			(looking-at "\\(cut\\|end\\)\\>"))
		    (if (or (nth 3 state) (nth 4 state) ignore-max)
			nil		; Doing a chunk only
		      (setq warning-message "=cut is not preceded by a POD section")
		      (or (car err-l) (setcar err-l (point))))
		  (beginning-of-line)

		  (setq b (point)
			bb b
			tb (match-beginning 0)
			b1 nil)		; error condition
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (or (re-search-forward "^\n=\\(cut\\|end\\)\\>" stop-point 'toend)
		      (progn
			(goto-char b)
			(if (re-search-forward "\n=\\(cut\\|end\\)\\>" stop-point 'toend)
			    (progn
			      (setq warning-message "=cut is not preceded by an empty line")
			      (setq b1 t)
			      (or (car err-l) (setcar err-l b))))))
		  (beginning-of-line 2)	; An empty line after =cut is not POD!
		  (setq e (point))
		  (and (> e max)
		       (progn
			 (remove-text-properties
			  max e '(syntax-type t in-pod t syntax-table t
					      attrib-group t
					      REx-interpolated t
					      cperl-postpone t
					      syntax-subtype t
					      here-doc-group t
					      rear-nonsticky t
					      front-sticky t
					      first-format-line t
					      REx-part2 t
					      indentable t))
			 (setq tmpend tb)))
		  (put-text-property b e 'in-pod t)
		  (put-text-property b e 'syntax-type 'in-pod)
		  (goto-char b)
		  (while (re-search-forward "\n\n[ \t]" e t)
		    ;; We start 'pod 1 char earlier to include the preceding line
		    (beginning-of-line)
		    (put-text-property (cperl-1- b) (point) 'syntax-type 'pod)
		    (cperl-put-do-not-fontify b (point) t)
		    ;; mark the non-literal parts as PODs
		    (if cperl-pod-here-fontify
			(cperl-postpone-fontification b (point) 'face face t))
		    (re-search-forward "\n\n[^ \t\f\n]" e 'toend)
		    (beginning-of-line)
		    (setq b (point)))
		  (put-text-property (cperl-1- (point)) e 'syntax-type 'pod)
		  (cperl-put-do-not-fontify (point) e t)
		  (if cperl-pod-here-fontify
		      (progn
			;; mark the non-literal parts as PODs
			(cperl-postpone-fontification (point) e 'face face t)
			(goto-char bb)
			(if (looking-at
			     "=[a-zA-Z0-9_]+\\>[ \t]*\\(\\(\n?[^\n]\\)+\\)$")
			    ;; mark the headers
			    (cperl-postpone-fontification
			     (match-beginning 1) (match-end 1)
			     'face head-face))
			(while (re-search-forward
				;; One paragraph
				"^\n=[a-zA-Z0-9_]+\\>[ \t]*\\(\\(\n?[^\n]\\)+\\)$"
				e 'toend)
			  ;; mark the headers
			  (cperl-postpone-fontification
			   (match-beginning 1) (match-end 1)
			   'face head-face))))
		  (cperl-commentify bb e nil)
		  (goto-char e)
		  (or (eq e (point-max))
		      (forward-char -1)))) ; Prepare for immediate POD start.
	       ;; Here document
	       ;; We can do many here-per-line;
	       ;; but multiline quote on the same line as <<HERE confuses us...
               ;; One extra () before this:
	       ;;  "<<\\(~?\\)"		 ; HERE-DOC, indented-p = capture 2
	       ;; First variant "BLAH" or just ``:  capture groups 4 and 5
	       ;; Second variant: Identifier or \ID: capture group 6 and 7
               ((match-beginning 3)     ; 2 + 1: found "<<", detect its type
                (let* ((matched-pos (match-beginning 0))
                       (quoted-delim-p (if (match-beginning 6) nil t))
                       (delim-capture (if quoted-delim-p 5 6)))
                  (when (cperl-is-here-doc-p matched-pos)
                    (let ((here-doc-results
                           (cperl-process-here-doc
                            min max end overshoot stop-point ; for recursion
                            end-of-here-doc err-l            ; for recursion
                            (equal (match-string 2) "~")     ; indented here-doc?
                            matched-pos                      ; for recovery (?)
                            (match-end 3)                    ; todo from here
                            (match-beginning delim-capture)  ; starting delimiter
                            (match-end delim-capture))))     ;   boundaries
                      (setq tmpend (nth 0 here-doc-results)
                            overshoot (nth 1 here-doc-results))
                      (and (nth 2 here-doc-results)
                           (setq warning-message (nth 2 here-doc-results)))))))
	       ;; format capture groups 8-9
	       ((match-beginning 8)
		(setq b (point)
		      name (if (match-beginning 9) ; 7 + 2
                               (match-string-no-properties 9)        ; 7 + 2
			     "")
		      tb (match-beginning 0))
		(setq argument nil)
                (put-text-property (line-beginning-position)
                                   b 'first-format-line 't)
		(if cperl-pod-here-fontify
		    (while (and (eq (forward-line) 0)
				(not (looking-at "^[.;]$")))
		      (cond
		       ((looking-at "^#")) ; Skip comments
		       ((and argument	; Skip argument multi-lines
			     (looking-at "^[ \t]*{"))
			(forward-sexp 1)
			(setq argument nil))
		       (argument	; Skip argument lines
			(setq argument nil))
		       (t		; Format line
			(setq b1 (point))
			(setq argument (looking-at "^[^\n]*[@^]"))
			(end-of-line)
			;; Highlight the format line
			(cperl-postpone-fontification b1 (point)
                                                      'face 'font-lock-string-face)
			(cperl-commentify b1 (point) nil)
			(cperl-put-do-not-fontify b1 (point) t))))
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (re-search-forward "^[.;]$" stop-point 'toend))
		(beginning-of-line)
		(if (looking-at "^\\.$") ; ";" is not supported yet
		    (progn
		      ;; Highlight the ending delimiter
		      (cperl-postpone-fontification (point) (+ (point) 1)
                                                    'face 'font-lock-string-face)
		      (cperl-commentify (point) (+ (point) 1) nil)
		      (cperl-put-do-not-fontify (point) (+ (point) 1) t))
		  (setq warning-message
                        (format "End of format `%s' not found." name))
		  (or (car err-l) (setcar err-l b)))
		(forward-line)
		(if (> (point) max)
		    (setq tmpend tb))
		(put-text-property b (point) 'syntax-type 'format))
	       ;; quotelike operator or regexp: capture groups 10 or 11
               ;; matches some false positives, to be eliminated here
	       ((or (match-beginning 10) (match-beginning 11))
		(setq b1 (if (match-beginning 10) 10 11)
		      argument (buffer-substring
				(match-beginning b1) (match-end b1))
		      b (point)		; end of qq etc
		      i b
		      c (char-after (match-beginning b1))
		      bb (char-after (1- (match-beginning b1))) ; tmp holder
		      ;; bb == "Not a stringy"
		      bb (if (eq b1 10) ; user variables/whatever
                             (or
                              ; false positive: "y_" has no word boundary
                              (save-match-data (looking-at "_"))
			      (and (memq bb (append "$@%*#_:-&>" nil)) ; $#y)
				   (cond ((eq bb ?-) (eq c ?s)) ; -s file test
					 ((eq bb ?\:) ; $opt::s
					  (eq (char-after
					       (- (match-beginning b1) 2))
					      ?\:))
					 ((eq bb ?\>) ; $foo->s
					  (eq (char-after
					       (- (match-beginning b1) 2))
					      ?\-))
					 ((eq bb ?\&)
					  (not (eq (char-after ; &&m/blah/
						    (- (match-beginning b1) 2))
						   ?\&)))
					 (t t))))
			   ;; <file> or <$file>
			   (and (eq c ?\<)
                                ;; Stringify what looks like a glob, but
				;; do not stringify file handles <FH>, <$fh> :
				(save-match-data
				  (looking-at
                                   (rx (sequence (opt "$")
                                                 (eval cperl--normal-identifier-rx)))))))
		      tb (match-beginning 0))
		(goto-char (match-beginning b1))
		(cperl-backward-to-noncomment (point-min))
		(or bb
		    (if (eq b1 11)	; bare /blah/ or <foo>
			(setq argument ""
			      b1 nil
			      bb	; Not a regexp?
			      (not
			       ;; What is below: regexp-p?
			       (and
				(or (memq (preceding-char)
					  (append (if (char-equal c ?\<)
						      ;; $a++ ? 1 : 2
						      "~{(=|&*!,;:["
						    "~{(=|&+-*!,;:[") nil))
				    (and (eq (preceding-char) ?\})
					 (cperl-after-block-p (point-min)))
				    (and (eq (char-syntax (preceding-char)) ?w)
					 (save-excursion
					   (forward-sexp -1)
                                           ;; After these keywords `/'
                                           ;; starts a RE.  One should
                                           ;; add all the
                                           ;; functions/builtins which
                                           ;; expect an argument, but
                                           ;; ...
					     (and
					      (not (memq (preceding-char)
							 '(?$ ?@ ?& ?%)))
					      (looking-at
                                               (regexp-opt
                                                '("while" "if" "unless"
                                                  "until" "for" "foreach"
                                                  "and" "or" "not"
					          "xor" "split" "grep" "map"
                                                  "print" "say" "return")
                                                'symbols)))))
				    (and (eq (preceding-char) ?.)
					 (eq (char-after (- (point) 2)) ?.))
				    (bobp))
				;; { $a++ / $b } doesn't start a regex, nor does $a--
				(not (and (memq (preceding-char) '(?+ ?-))
					  (eq (preceding-char) (char-before (1- (point))))))
				;;  m|blah| ? foo : bar;
				(not
				 (and (eq c ?\?)
				      cperl-use-syntax-table-text-property
				      (not (bobp))
				      (progn
					(forward-char -1)
					(looking-at "\\s|"))))))
			      b (1- b))
		      ;; s y tr m
		      ;; Check for $a -> y
		      (setq b1 (preceding-char)
			    go (point))
		      (if (and (eq b1 ?>)
			       (eq (char-after (- go 2)) ?-))
			  ;; Not a regexp
			  (setq bb t))))
		(or bb
		    (progn
		      (goto-char b)
		      (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
			  (goto-char (match-end 0))
			(skip-chars-forward " \t\n\f"))
		      (cond ((and (eq (following-char) ?\})
				  (eq b1 ?\{))
			     ;; Check for $a[23]->{ s }, @{s} and *{s::foo}
			     (goto-char (1- go))
			     (skip-chars-backward " \t\n\f")
			     (if (memq (preceding-char) (append "$@%&*" nil))
				 (setq bb t) ; @{y}
			       (condition-case nil
				   (forward-sexp -1)
				 (error nil)))
			     (if (or bb
				     (looking-at ; $foo -> {s}
                                      (rx
                                       (sequence
                                        (in "$@") (0+ "$")
                                        (or
                                         (eval cperl--normal-identifier-rx)
                                         (not (in "{")))
                                        (opt (sequence (eval cperl--ws*-rx))
                                             "->")
                                        (eval cperl--ws*-rx)
                                        "{")))
				     (and ; $foo[12] -> {s}
				      (memq (following-char) '(?\{ ?\[))
				      (progn
					(forward-sexp 1)
					(looking-at "\\([ \t\n]*->\\)?[ \t\n]*{"))))
				 (setq bb t)
			       (goto-char b)))
			    ((and (eq (following-char) ?=)
				  (eq (char-after (1+ (point))) ?\>))
			     ;; Check for { foo => 1, s => 2 }
			     ;; Apparently s=> is never a substitution...
			     (setq bb t))
			    ((and (eq (following-char) ?:)
				  (eq b1 ?\{) ; Check for $ { s::bar }
				  ;;  (looking-at "::[a-zA-Z0-9_:]*[ \t\n\f]*}")
                                  (looking-at
                                   (rx (sequence "::"
                                                 (eval cperl--normal-identifier-rx)
                                                 (eval cperl--ws*-rx)
                                                 "}")))
				  (progn
				    (goto-char (1- go))
				    (skip-chars-backward " \t\n\f")
				    (memq (preceding-char)
					  (append "$@%&*" nil))))
			     (setq bb t))
			    ((eobp)
			     (setq bb t)))))
		(if bb
		    (goto-char i)
		  ;; Skip whitespace and comments...
		  (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
		      (goto-char (match-end 0))
		    (skip-chars-forward " \t\n\f"))
		  (if (> (point) b)
		      (put-text-property b (point) 'syntax-type 'prestring))
		  ;; qtag means two-arg matcher, may be reset to
		  ;;   2 or 3 later if some special quoting is needed.
		  ;; e1 means matching-char matcher.
		  (setq b (point)	; before the first delimiter
			;; has 2 args
			i2 (string-match "^\\([sy]\\|tr\\)$" argument)
			;; We do not search to max, since we may be called from
			;; some hook of fontification, and max is random
			i (cperl-forward-re stop-point end
					    i2
					    st-l err-l argument)
			;; If `go', then it is considered as 1-arg, `b1' is nil
			;; as in s/foo//x; the point is before final "slash"
			b1 (nth 1 i)	; start of the second part
			tag (nth 2 i)	; ender-char, true if second part
					; is with matching chars []
			go (nth 4 i)	; There is a 1-char part after the end
			i (car i)	; intermediate point
			e1 (point)	; end
			;; Before end of the second part if non-matching: ///
			tail (if (and i (not tag))
				 (1- e1))
			e (if i i e1)	; end of the first part
			qtag nil	; need to preserve backslashitis
			is-x-REx nil is-o-REx nil); REx has //x //o modifiers
		  ;; If s{} (), then b/b1 are at "{", "(", e1/i after ")", "}"
		  ;; Commenting \\ is dangerous, what about ( ?
		  (and i tail
		       (eq (char-after i) ?\\)
		       (setq qtag t))
		  (and (if go (looking-at ".\\sw*x")
			 (looking-at "\\sw*x")) ; qr//x
		       (setq is-x-REx t))
		  (and (if go (looking-at ".\\sw*o")
			 (looking-at "\\sw*o")) ; //o
		       (setq is-o-REx t))
		  (if (null i)
		      ;; Considered as 1arg form
		      (progn
			(cperl-commentify b (point) t)
			(put-text-property b (point) 'syntax-type 'string)
			(if (or is-x-REx
				;; ignore other text properties:
				(string-match "^qw$" argument))
			    (put-text-property b (point) 'indentable t))
			(and go
			     (setq e1 (cperl-1+ e1))
			     (or (eobp)
				 (forward-char 1))))
		    (cperl-commentify b i t)
		    (if (looking-at "\\sw*e") ; s///e
			(progn
			  ;; Cache the syntax info...
			  (setq cperl-syntax-state (cons state-point state))
			  (and
			   ;; silent:
			   (car (cperl-find-pods-heres b1 (1- (point)) t end))
			   ;; Error
			   (goto-char (1+ max)))
			  (if (and tag (eq (preceding-char) ?\>))
			      (progn
				(cperl-modify-syntax-type (1- (point)) cperl-st-ket)
				(cperl-modify-syntax-type i cperl-st-bra)))
			  (put-text-property b i 'syntax-type 'string)
			  (put-text-property i (point) 'syntax-type 'multiline)
			  (if is-x-REx
			      (put-text-property b i 'indentable t)))
		      (cperl-commentify b1 (point) t)
		      (put-text-property b (point) 'syntax-type 'string)
		      (if is-x-REx
			  (put-text-property b i 'indentable t))
		      (if qtag
			  (cperl-modify-syntax-type (1+ i) cperl-st-punct))
		      (setq tail nil)))
		  ;; Now: tail: if the second part is non-matching without ///e
		  (if (eq (char-syntax (following-char)) ?w)
		      (progn
			(forward-word-strictly 1) ; skip modifiers s///s
			(if tail (cperl-commentify tail (point) t))
			(cperl-postpone-fontification
			 e1 (point) 'face my-cperl-REx-modifiers-face)))
		  ;; Check whether it is m// which means "previous match"
		  ;; and highlight differently
		  (setq is-REx
			(and (string-match "^\\([sm]?\\|qr\\)$" argument)
			     (or (not (= (length argument) 0))
				 (not (eq c ?\<)))))
		  (if (and is-REx
			   (eq e (+ 2 b))
			   ;; split // *is* using zero-pattern
			   (save-excursion
			     (condition-case nil
				 (progn
				   (goto-char tb)
				   (forward-sexp -1)
				   (not (looking-at "split\\>")))
			       (error t))))
		      (cperl-postpone-fontification
                       b e 'face 'font-lock-warning-face)
		    (if (or i2		; Has 2 args
			    (and cperl-fontify-m-as-s
				 (or
				  (string-match "^\\(m\\|qr\\)$" argument)
				  (and (eq 0 (length argument))
				       (not (eq ?\< (char-after b)))))))
			(progn
			  (cperl-postpone-fontification
			   b (cperl-1+ b) 'face my-cperl-delimiters-face)
			  (cperl-postpone-fontification
			   (1- e) e 'face my-cperl-delimiters-face)))
		    (if (and is-REx cperl-regexp-scan)
			;; Process RExen: embedded comments, charclasses and ]
                        ;; Examples:
                        ;;/\3333\xFg\x{FFF}a\ppp\PPP\qqq\C\99f(?{  foo  })(??{  foo  })/;
                        ;;/a\.b[^a[:ff:]b]x$ab->$[|$,$ab->[cd]->[ef]|$ab[xy].|^${a,b}{c,d}/;
                        ;;/(?<=foo)(?<!bar)(x)(?:$ab|\$\/)$|\\\b\x888\776\[\:$/xxx;
                        ;;m?(\?\?{b,a})? + m/(??{aa})(?(?=xx)aa|bb)(?#aac)/;
                        ;;m$(^ab[c]\$)$ + m+(^ab[c]\$\+)+ + m](^ab[c\]$|.+)] + m)(^ab[c]$|.+\));
                        ;;m^a[\^b]c^ + m.a[^b]\.c.;
			(save-excursion
			  (goto-char (1+ b))
			  ;; First
			  (cperl-look-at-leading-count is-x-REx e)
			  (setq hairy-RE
				(concat
				 (if is-x-REx
				     (if (eq (char-after b) ?\#)
					 "\\((\\?\\\\#\\)\\|\\(\\\\#\\)"
				       "\\((\\?#\\)\\|\\(#\\)")
				   ;; keep the same count: add a fake group
				   (if (eq (char-after b) ?\#)
				       "\\((\\?\\\\#\\)\\(\\)"
				     "\\((\\?#\\)\\(\\)"))
				 "\\|"
				    "\\(\\[\\)" ; 3=[
				 "\\|"
				    "\\(]\\)" ; 4=]
				 "\\|"
				 ;; XXXX Will not be able to use it in s)))
				 (if (eq (char-after b) ?\) )
				     "\\())))\\)" ; Will never match
				   (if (eq (char-after b) ?? )
				       ;;"\\((\\\\\\?\\(\\\\\\?\\)?{\\)"
				       "\\((\\\\\\?\\\\\\?{\\|()\\\\\\?{\\)"
				     "\\((\\?\\??{\\)")) ; 5= (??{ (?{
				 "\\|"	; 6= 0-length, 7: name, 8,9:code, 10:group
				    "\\(" ;; XXXX 1-char variables, exc. |()\s
				       "[$@]"
				       "\\("
                                          (rx (eval cperl--normal-identifier-rx))
				       "\\|"
				          "{[^{}]*}" ; only one-level allowed
				       "\\|"
				          "[^{(|) \t\r\n\f]"
				       "\\)"
				       "\\(" ;;8,9:code part of array/hash elt
				          "\\(" "->" "\\)?"
				          "\\[[^][]*\\]"
					  "\\|"
				          "{[^{}]*}"
				       "\\)*"
				    ;; XXXX: what if u is delim?
				    "\\|"
				       "[)^|$.*?+]"
				    "\\|"
				       "{[0-9]+}"
				    "\\|"
				       "{[0-9]+,[0-9]*}"
				    "\\|"
				       "\\\\[luLUEQbBAzZG]"
				    "\\|"
				       "(" ; Group opener
				       "\\(" ; 10 group opener follower
				          "\\?\\((\\?\\)" ; 11: in (?(?=C)A|B)
				       "\\|"
				          "\\?[:=!>?{]"	; "?" something
				       "\\|"
				          "\\?[-imsx]+[:)]" ; (?i) (?-s:.)
				       "\\|"
				          "\\?([0-9]+)"	; (?(1)foo|bar)
				       "\\|"
					  "\\?<[=!]"
				       "\\)?"
				    "\\)"
				 "\\|"
				    "\\\\\\(.\\)" ; 12=\SYMBOL
				 ))
			  (while
			      (and (< (point) (1- e))
				   (re-search-forward hairy-RE (1- e) 'to-end))
			    (goto-char (match-beginning 0))
			    (setq REx-subgr-start (point)
				  was-subgr (following-char))
			    (cond
			     ((match-beginning 6) ; 0-length builtins, groups
			      (goto-char (match-end 0))
			      (if (match-beginning 11)
				  (goto-char (match-beginning 11)))
			      (if (>= (point) e)
				  (goto-char (1- e)))
			      (cperl-postpone-fontification
			       (match-beginning 0) (point)
			       'face
			       (cond
				((eq was-subgr ?\) )
				 (condition-case nil
				     (save-excursion
				       (forward-sexp -1)
				       (if (> (point) b)
					   (if (if (eq (char-after b) ?? )
						   (looking-at "(\\\\\\?")
						 (eq (char-after (1+ (point))) ?\?))
					       my-cperl-REx-0length-face
					     my-cperl-REx-ctl-face)
                                         'font-lock-warning-face))
                                   (error 'font-lock-warning-face)))
				((eq was-subgr ?\| )
				 my-cperl-REx-ctl-face)
				((eq was-subgr ?\$ )
				 (if (> (point) (1+ REx-subgr-start))
				     (progn
				       (put-text-property
					(match-beginning 0) (point)
					'REx-interpolated
					(if is-o-REx 0
					    (if (and (eq (match-beginning 0)
							 (1+ b))
						     (eq (point)
							 (1- e))) 1 t)))
                                       'font-lock-variable-name-face)
				   my-cperl-REx-spec-char-face))
				((memq was-subgr (append "^." nil) )
				 my-cperl-REx-spec-char-face)
				((eq was-subgr ?\( )
				 (if (not (match-beginning 10))
				     my-cperl-REx-ctl-face
				   my-cperl-REx-0length-face))
				(t my-cperl-REx-0length-face)))
			      (if (and (memq was-subgr (append "(|" nil))
				       (not (string-match "(\\?[-imsx]+)"
							  (match-string 0))))
				  (cperl-look-at-leading-count is-x-REx e))
			      (setq was-subgr nil)) ; We do stuff here
			     ((match-beginning 12) ; \SYMBOL
			      (forward-char 2)
			      (if (>= (point) e)
				  (goto-char (1- e))
				;; How many chars to not highlight:
				;; 0-len special-alnums in other branch =>
				;; Generic:  \non-alnum (1), \alnum (1+face)
				;; Is-delim: \non-alnum (1/spec-2) alnum-1 (=what hai)
				(setq REx-subgr-start (point)
				      qtag (preceding-char))
				(cperl-postpone-fontification
				 (- (point) 2) (- (point) 1) 'face
				 (if (memq qtag
				           (append "gijkmoqFIJKMOTY" nil))
                                     'font-lock-warning-face
				   my-cperl-REx-0length-face))
				(if (and (eq (char-after b) qtag)
					 (memq qtag (append ".])^$|*?+" nil)))
				    (progn
				      (if (and cperl-use-syntax-table-text-property
					       (eq qtag ?\) ))
					  (put-text-property
					   REx-subgr-start (1- (point))
					   'syntax-table cperl-st-punct))
				      (cperl-postpone-fontification
				       (1- (point)) (point) 'face
					; \] can't appear below
				       (if (memq qtag (append ".]^$" nil))
					   'my-cperl-REx-spec-char-face
					 (if (memq qtag (append "*?+" nil))
					     'my-cperl-REx-0length-face
					   'my-cperl-REx-ctl-face))))) ; )|
				;; Test for arguments:
				(cond
				 ;; This is not pretty: the 5.8.7 logic:
				 ;; \0numx  -> octal (up to total 3 dig)
				 ;; \DIGIT  -> backref unless \0
				 ;; \DIGITs -> backref if valid
				 ;;	     otherwise up to 3 -> octal
				 ;; Do not try to distinguish, we guess
				 ((or (and (memq qtag (append "01234567" nil))
					   (re-search-forward
					    "\\=[01234567]?[01234567]?"
					    (1- e) 'to-end))
				      (and (memq qtag (append "89" nil))
					   (re-search-forward
					    "\\=[0123456789]*" (1- e) 'to-end))
				      (and (eq qtag ?x)
					   (re-search-forward
					    "\\=[[:xdigit:]][[:xdigit:]]?\\|\\={[[:xdigit:]]+}"
					    (1- e) 'to-end))
				      (and (memq qtag (append "pPN" nil))
					   (re-search-forward "\\={[^{}]+}\\|."
					    (1- e) 'to-end))
				      (eq (char-syntax qtag) ?w))
				  (cperl-postpone-fontification
				   (1- REx-subgr-start) (point)
				   'face my-cperl-REx-length1-face))))
			      (setq was-subgr nil)) ; We do stuff here
			     ((match-beginning 3) ; [charclass]
			      ;; Highlight leader, trailer, POSIX classes
			      (forward-char 1)
			      (if (eq (char-after b) ?^ )
				  (and (eq (following-char) ?\\ )
				       (eq (char-after (cperl-1+ (point)))
					   ?^ )
				       (forward-char 2))
				(and (eq (following-char) ?^ )
				     (forward-char 1)))
			      (setq argument b ; continue? & end of last POSIX
				    tag nil ; list of POSIX classes
				    qtag (point)) ; after leading ^ if present
			      (if (eq (char-after b) ?\] )
				  (and (eq (following-char) ?\\ )
				       (eq (char-after (cperl-1+ (point)))
					   ?\] )
				       (setq qtag (1+ qtag))
				       (forward-char 2))
				(and (eq (following-char) ?\] )
				     (forward-char 1)))
			      (setq REx-subgr-end qtag)	;End smart-highlighted
			      ;; Apparently, I can't put \] into a charclass
			      ;; in m]]: m][\\\]\]] produces [\\]]
                              ;; POSIX?  [:word:] [:^word:] only inside []
                              ;; "\\=\\(\\\\.\\|[^][\\]\\|\\[:\\^?\sw+:]\\|\\[[^:]\\)*]")
			      (while	; look for unescaped ]
				  (and argument
				       (re-search-forward
					(if (eq (char-after b) ?\] )
					    "\\=\\(\\\\[^]]\\|[^]\\]\\)*\\\\]"
					  "\\=\\(\\\\.\\|[^]\\]\\)*]")
					(1- e) 'toend))
				;; Is this ] an end of POSIX class?
				(if (save-excursion
				      (and
				       (search-backward "[" argument t)
				       (< REx-subgr-start (point))
				       (setq argument (point)) ; POSIX-start
				       (or ; Should work with delim = \
					(not (eq (preceding-char) ?\\ ))
					;; XXXX Double \\ is needed with 19.33
					(cl-evenp (skip-chars-backward "\\\\")))
				       (looking-at
					(cond
					 ((eq (char-after b) ?\] )
					  "\\\\*\\[:\\^?\\sw+:\\\\\\]")
					 ((eq (char-after b) ?\: )
					  "\\\\*\\[\\\\:\\^?\\sw+\\\\:]")
					 ((eq (char-after b) ?^ )
					  "\\\\*\\[:\\(\\\\\\^\\)?\\sw+:]")
					 ((eq (char-syntax (char-after b))
					      ?w)
					  (concat
					   "\\\\*\\[:\\(\\\\\\^\\)?\\(\\\\"
					   (char-to-string (char-after b))
					   "\\|\\sw\\)+:]"))
					 (t "\\\\*\\[:\\^?\\sw*:]")))
				       (goto-char REx-subgr-end)
				       (cperl-highlight-charclass
					argument my-cperl-REx-spec-char-face
					my-cperl-REx-0length-face my-cperl-REx-length1-face)))
				    (setq tag (cons (cons argument (point))
						    tag)
					  argument (point)
					  REx-subgr-end argument) ; continue
				  (setq argument nil)))
			      (and argument
				   (setq warning-message
                                         (format "Couldn't find end of charclass in a REx, pos=%s"
					         REx-subgr-start)))
			      (setq argument (1- (point)))
			      (goto-char REx-subgr-end)
			      (cperl-highlight-charclass
			       argument my-cperl-REx-spec-char-face
			       my-cperl-REx-0length-face my-cperl-REx-length1-face)
			      (forward-char 1)
			      ;; Highlight starter, trailer, POSIX
			      (if (and cperl-use-syntax-table-text-property
				       (> (- (point) 2) REx-subgr-start))
				  (put-text-property
				   (1+ REx-subgr-start) (1- (point))
				   'syntax-table cperl-st-punct))
			      (cperl-postpone-fontification
			       REx-subgr-start qtag
			       'face my-cperl-REx-spec-char-face)
			      (cperl-postpone-fontification
			       (1- (point)) (point) 'face
			       my-cperl-REx-spec-char-face)
			      (if (eq (char-after b) ?\] )
				  (cperl-postpone-fontification
				   (- (point) 2) (1- (point))
				   'face my-cperl-REx-0length-face))
			      (while tag
				(cperl-postpone-fontification
				 (car (car tag)) (cdr (car tag))
                                 'face 'font-lock-variable-name-face) ;my-cperl-REx-length1-face
				(setq tag (cdr tag)))
			      (setq was-subgr nil)) ; did facing already
			     ;; Now rare stuff:
			     ((and (match-beginning 2) ; #-comment
				   (/= (match-beginning 2) (match-end 2)))
			      (beginning-of-line 2)
			      (if (> (point) e)
				  (goto-char (1- e))))
			     ((match-beginning 4) ; character "]"
			      (setq was-subgr nil) ; We do stuff here
			      (goto-char (match-end 0))
			      (if cperl-use-syntax-table-text-property
				  (put-text-property
				   (1- (point)) (point)
				   'syntax-table cperl-st-punct))
			      (cperl-postpone-fontification
			       (1- (point)) (point)
                               'face 'font-lock-warning-face))
			     ((match-beginning 5) ; before (?{}) (??{})
			      (setq tag (match-end 0))
			      (if (or (setq qtag
					    (cperl-forward-group-in-re st-l))
				      (and (>= (point) e)
					   (setq qtag "no matching `)' found"))
				      (and (not (eq (char-after (- (point) 2))
						    ?\} ))
					   (setq qtag "Can't find })")))
				  (progn
				    (goto-char (1- e))
				    (setq warning-message
                                          (format "%s" qtag)))
				(cperl-postpone-fontification
				 (1- tag) (1- (point))
                                 'face 'font-lock-variable-name-face)
				(cperl-postpone-fontification
				 REx-subgr-start (1- tag)
				 'face my-cperl-REx-spec-char-face)
				(cperl-postpone-fontification
				 (1- (point)) (point)
				 'face my-cperl-REx-spec-char-face)
				(if cperl-use-syntax-table-text-property
				    (progn
				      (put-text-property
				       (- (point) 2) (1- (point))
				       'syntax-table cperl-st-cfence)
				      (put-text-property
				       (+ REx-subgr-start 2)
				       (+ REx-subgr-start 3)
				       'syntax-table cperl-st-cfence))))
			      (setq was-subgr nil))
			     (t		; (?#)-comment
			      ;; Inside "(" and "\" aren't special in any way
			      ;; Works also if the outside delimiters are ().
			      (or;;(if (eq (char-after b) ?\) )
			       ;;(re-search-forward
			       ;; "[^\\]\\(\\\\\\\\\\)*\\\\)"
			       ;; (1- e) 'toend)
			       (search-forward ")" (1- e) 'toend)
			       ;;)
			       (setq warning-message
				     (format "Couldn't find end of (?#...)-comment in a REx, pos=%s"
				             REx-subgr-start)))))
			    (if (>= (point) e)
				(goto-char (1- e)))
			    (cond
			     (was-subgr
			      (setq REx-subgr-end (point))
			      (cperl-commentify
			       REx-subgr-start REx-subgr-end nil)
			      (cperl-postpone-fontification
			       REx-subgr-start REx-subgr-end
                               'face 'font-lock-comment-face))))))
		    (if (and is-REx is-x-REx)
			(put-text-property (1+ b) (1- e)
					   'syntax-subtype 'x-REx)))
		  (if (and i2 e1 (or (not b1) (> e1 b1)))
		      (progn		; No errors finding the second part...
			(cperl-postpone-fontification
			 (1- e1) e1 'face my-cperl-delimiters-face)
			(if (and (not (eobp))
				 (assoc (char-after b) cperl-starters))
			    (progn
			      (cperl-postpone-fontification
			       b1 (1+ b1) 'face my-cperl-delimiters-face)
			      (put-text-property b1 (1+ b1)
					   'REx-part2 t)))))
		  (if (> (point) max)
		      (setq tmpend tb))))
	       ((match-beginning 14)	; sub with prototype or attribute
		;; 1+6+2+1+1=11 extra () before this (sub with proto/attr):
		;; match-string 12: Keyword "sub" or "method"
		;; match-string 13: Name of the subroutine (optional)
                ;; match-string 14: Indicator for proto/attr/signature
                ;; match-string 15: Prototype
                ;; match-string 16: Subroutine signature
                ;; match-string 17: Distinguish declaration/definition
                (setq b1 (match-beginning 13) e1 (match-end 13))
		(if (memq (char-after (1- b))
			  '(?\$ ?\@ ?\% ?\& ?\*))
		    nil ;; we found $sub or @method etc
		  (goto-char b)
		  (if (match-beginning 15) ; a complete prototype
		      (progn
			(cperl-commentify ; Prototypes; mark as string
			 (match-beginning 15) (match-end 15) t)
			(goto-char (match-end 0))
			;; Now look for attributes after prototype:
			(forward-comment (buffer-size))
			(cperl-find-sub-attrs st-l b1 e1 b))
		    ;; treat attributes without prototype and incomplete stuff
                    (if (match-beginning 16) ; a complete subroutine signature
                        ;; A signature ending in "$)" must not be
                        ;; mistaken as the punctuation variable $) which
                        ;; messes up balance of parens (Bug#74245).
                        (progn
                          (when (= (char-after (- (match-end 16) 2)) ?$)
                            (put-text-property (- (match-end 16) 2) (1- (match-end 16))
                                               'syntax-table cperl-st-punct))
                          (goto-char (match-end 16)))
		      (goto-char (match-beginning 17))
		      (cperl-find-sub-attrs st-l b1 e1 b)))))
	        ;; 1+6+2+1+1+6+1=18 extra () before this:
	       ;;    "\\(\\<sub[ \t\n\f]+\\|[&*$@%]\\)[a-zA-Z0-9_]*'")
	       ((match-beginning 19)	; old $abc'efg syntax
		(setq bb (match-end 0))
		(put-text-property (1- bb) bb 'syntax-table cperl-st-word)
		(goto-char bb))
	       ;; 1+6+2+1+1+6+1+1=19 extra () before this:
	       ;; "__\\(END\\|DATA\\)__"
	       ((match-beginning 20)	; __END__, __DATA__
                (if (eq cperl-fontify-trailer 'perl-code)
		    (setq bb (match-end 0))
                  (setq bb (point-max)))
		(cperl-commentify b bb nil)
		(setq end t))
	       ;; "\\\\\\(['`\"($]\\)"
	       ((match-beginning 21)
		;; Trailing backslash; make non-quoting outside string/comment
		(setq bb (match-end 0))
		(goto-char b)
		(skip-chars-backward "\\\\")
		;; (setq i2 (= (% (skip-chars-backward "\\\\") 2) -1))
		(cperl-modify-syntax-type b cperl-st-punct)
		(goto-char bb))
	       (t (error "Error in regexp of the sniffer")))
	      (if (> (point) stop-point)
		  (progn
		    (if end
			(setq warning-message "Garbage after __END__/__DATA__ ignored")
		      (setq warning-message "Unbalanced syntax found while scanning")
		      (or (car err-l) (setcar err-l b)))
		    (goto-char stop-point))))
	    (setq cperl-syntax-state (cons state-point state)
		  ;; Do not mark syntax as done past tmpend???
		  cperl-syntax-done-to (or tmpend (max (point) max)))
	    ;;(message "state-at=%s, done-to=%s" state-point cperl-syntax-done-to)
	    )
	  (if (car err-l) (goto-char (car err-l))
	    (or non-inter
		(message "Scanning for \"hard\" Perl constructions... done"))))
      (and (buffer-modified-p)
	   (not modified)
	   (set-buffer-modified-p nil))
      ;; I do not understand what this is doing here.  It breaks font-locking
      ;; because it resets the syntax-table from font-lock-syntax-table to
      ;; cperl-mode-syntax-table.
      ;; (set-syntax-table cperl-mode-syntax-table)
      )
    (when warning-message (message warning-message))
    (list (car err-l) overshoot)))

(defun cperl-find-pods-heres-region (min max)
  "Call `cperl-find-pods-heres'  in the region between MIN and MAX."
  (interactive "r")
  (cperl-find-pods-heres min max))

(defun cperl-backward-to-noncomment (lim)
  "Go backward.  Stop at LIM or after non-whitespace not in a comment."
  ;;XXXX Wrongly understands end-of-multiline strings with # as comment"
  (let (stop p pr)
    (while (and (not stop) (> (point) (or lim (point-min))))
      (skip-chars-backward " \t\n\f" lim)
      (setq p (point))
      (beginning-of-line)
      (if (memq (setq pr (get-text-property (point) 'syntax-type))
		'(pod here-doc here-doc-delim))
	  (progn
	    (cperl-unwind-to-safe nil)
	    (setq pr (get-text-property (point) 'syntax-type))))
      (or (and (looking-at "^[ \t]*\\(#\\|$\\)")
	       (not (memq pr '(string prestring))))
	  (progn (cperl-to-comment-or-eol) (bolp))
	  (progn
	    (skip-chars-backward " \t")
	    (if (< p (point)) (goto-char p))
	    (setq stop t))))))

;; Used only in `cperl-sniff-for-indent'...
(defun cperl-block-p ()
  "Return non-nil if this is the start of a block.  Point is before ?\\{."
  ;; No save-excursion!  This is more a distinguisher of a block/hash ref...
  (cperl-backward-to-noncomment (point-min))
  (or (memq (preceding-char) (append ";){}$@&%\C-@" nil)) ; Or label!  \C-@ at bobp
					; Label may be mixed up with `$blah :'
      (save-excursion (cperl-after-label))
      ;; text with the 'attrib-group property is also covered by the
      ;; next clause.  We keep it because it is faster (for
      ;; subroutines with attributes).
      (get-text-property (cperl-1- (point)) 'attrib-group)
      (save-excursion (cperl-block-declaration-p))
      (and (memq (char-syntax (preceding-char)) '(?w ?_))
	   (progn
	     (backward-sexp)
	     ;; sub {BLK}, print {BLK} $data, but NOT `bless', `return', `tr', `constant'
             ;; a-zA-Z is fine here, these are Perl keywords
	     (or (and (looking-at "[a-zA-Z0-9_:]+[ \t\n\f]*[{#]") ; Method call syntax
		      (not (looking-at "\\(bless\\|return\\|q[wqrx]?\\|tr\\|[smy]\\|constant\\)\\>")))
		 ;; sub bless::foo {}
		 (progn
		   (cperl-backward-to-noncomment (point-min))
		   (and (eq (preceding-char) ?b)
			(progn
			  (forward-sexp -1)
			  (looking-at (concat cperl-sub-regexp "[ \t\n\f#]"))))))))))

;; What is the difference of (cperl-after-block-p lim t) and (cperl-block-p)?
;; No save-excursion; condition-case ...  In (cperl-block-p) the block
;; may be a part of an in-statement construct, such as
;;   ${something()}, print {FH} $data.
;; Moreover, one takes positive approach (looks for else,grep etc)
;; another negative (looks for bless,tr etc)
(defun cperl-after-block-p (lim &optional pre-block)
  "Return non-nil if the preceding } (if PRE-BLOCK, following {) delimits a block.
Would not look before LIM.  Assumes that LIM is a good place to begin a
statement.  The kind of block we treat here is one after which a new
statement would start; thus the block in ${func()} does not count."
  (save-excursion
    (condition-case nil
	(progn
	  (or pre-block (forward-sexp -1))
	  (cperl-backward-to-noncomment lim)
	  (or (eq (point) lim)
	      ;; if () {}   // sub f () {}   // sub f :a(') {}
	      (eq (preceding-char) ?\) )
	      ;; label: {}
	      (save-excursion (cperl-after-label))
	      ;; sub :attr {}
	      (get-text-property (cperl-1- (point)) 'attrib-group)
              (save-excursion (cperl-block-declaration-p))
	      (if (memq (char-syntax (preceding-char)) '(?w ?_)) ; else {}
		  (save-excursion
		    (forward-sexp -1)
		    ;; else {}     but not    else::func {}
		    (or (and (looking-at (rx (or "else" "catch" "try"
                                                 "finally" "defer"
                                                 "continue" "grep" "map"
                                                 "ADJUST" "BEGIN" "CHECK" "END"
                                                 "INIT" "UNITCHECK")))
			     (not (looking-at "\\(\\sw\\|_\\)+::")))
			;; sub f {}
			(progn
			  (cperl-backward-to-noncomment lim)
			  (and (cperl-char-ends-sub-keyword-p (preceding-char))
			       (progn
				 (forward-sexp -1)
				 (looking-at
                                  (concat cperl-sub-regexp "[ \t\n\f#]")))))))
		;; What precedes is not word...  XXXX Last statement in sub???
		(cperl-after-expr-p lim))))
      (error nil))))

(defun cperl-after-expr-p (&optional lim chars test)
  "Return non-nil if the position is good for start of expression.
TEST is the expression to evaluate at the found position.  If absent,
CHARS is a string that contains good characters to have before
us (however, `}' is treated \"smartly\" if it is not in the list).  LIM
is the minimal position to use."
  (let ((lim (or lim (point-min)))
	stop p)
    (cperl-update-syntaxification (point))
    (save-excursion
      (while (and (not stop) (> (point) lim))
	(skip-chars-backward " \t\n\f" lim)
	(setq p (point))
	(beginning-of-line)
	;;(memq (setq pr (get-text-property (point) 'syntax-type))
	;;      '(pod here-doc here-doc-delim))
	(if (get-text-property (point) 'here-doc-group)
	    (progn
	      (goto-char
	       (cperl-beginning-of-property (point) 'here-doc-group))
	      (beginning-of-line 0)))
	(if (get-text-property (point) 'in-pod)
	    (progn
	      (goto-char
	       (cperl-beginning-of-property (point) 'in-pod))
	      (beginning-of-line 0)))
	(if (looking-at "^[ \t]*\\(#\\|$\\)") nil ; Only comment, skip
	  ;; Else: last iteration, or a label
	  (cperl-to-comment-or-eol)	; Will not move past "." after a format
	  (skip-chars-backward " \t")
	  (if (< p (point)) (goto-char p))
	  (setq p (point))
	  (if (and (eq (preceding-char) ?:)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n\f" lim)
		     (memq (char-syntax (preceding-char)) '(?w ?_))))
	      (forward-sexp -1)		; Possibly label.  Skip it
	    (goto-char p)
	    (setq stop t))))
      (or (bobp)			; ???? Needed
	  (eq (point) lim)
	  (looking-at "[ \t]*__\\(END\\|DATA\\)__") ; After this anything goes
	  (progn
	    (if test (eval test)
	      (or (memq (preceding-char) (append (or chars "{;") nil))
		  (and (eq (preceding-char) ?\})
		       (cperl-after-block-p lim))
		  (and (eq (following-char) ?.)	; in format: see comment above
		       (eq (get-text-property (point) 'syntax-type)
			   'format)))))))))

(defun cperl-backward-to-start-of-expr (&optional lim)
  "Go backward to the start of the expression, but not before LIM."
  (condition-case nil
      (progn
	(while (and (or (not lim)
			(> (point) lim))
		    (not (cperl-after-expr-p lim)))
	  (forward-sexp -1)
	  ;; May be after $, @, $# etc of a variable
	  (skip-chars-backward "$@%#")))
    (error nil)))

(defun cperl-at-end-of-expr (&optional lim)
  "Find the end of the previous expression.  Do not go back beyond LIM."
  ;; Since the SEXP approach below is very fragile, do some overengineering
  (or (looking-at (concat cperl-maybe-white-and-comment-rex "[;}]"))
      (condition-case nil
	  (save-excursion
	    ;; If nothing interesting after, does as (forward-sexp -1);
	    ;; otherwise fails, or ends at a start of following sexp.
	    ;; XXXX PROBLEMS: if what follows (after ";") @FOO, or ${bar}
	    ;; may be stuck after @ or $; just put some stupid workaround now:
	    (let ((p (point)))
	      (forward-sexp 1)
	      (forward-sexp -1)
	      (while (memq (preceding-char) (append "%&@$*" nil))
		(forward-char -1))
	      (or (< (point) p)
		  (cperl-after-expr-p lim))))
	(error t))))

(defun cperl-forward-to-end-of-expr (&optional lim)
  "Go forward to the end of the expression, but not beyond LIM."
  (condition-case nil
      (progn
	(while (and (< (point) (or lim (point-max)))
		    (not (cperl-at-end-of-expr)))
	  (forward-sexp 1)))
    (error nil)))

(defun cperl-backward-to-start-of-continued-exp (lim)
  "Go backward to the start of a continuation line, but not beyond LIM."
  (if (memq (preceding-char) (append ")]}\"'`" nil))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun cperl-after-block-and-statement-beg (lim)
  "Return non-nil if the preceding ?} ends the statement.
Do not look before LIM."
  ;;  We assume that we are after ?\}
  (and
   (cperl-after-block-p lim)
   (save-excursion
     (forward-sexp -1)
     (cperl-backward-to-noncomment (point-min))
     (or (bobp)
	 (eq (point) lim)
	 (not (= (char-syntax (preceding-char)) ?w))
	 (progn
	   (forward-sexp -1)
	   (not
	    (looking-at
	     "\\(map\\|grep\\|say\\|printf?\\|system\\|exec\\|tr\\|s\\)\\_>")))))))


(defun cperl-indent-exp ()
  "Simple variant of indentation of continued-sexp.

Will not indent comment if it starts at `comment-indent' or looks like
continuation of the comment on the previous line.

If `cperl-indent-region-fix-constructs', will improve spacing on
conditional/loop constructs."
  (interactive)
  (save-excursion
    (let ((tmp-end (line-end-position)) top done)
      (save-excursion
	(beginning-of-line)
	(while (null done)
	  (setq top (point))
	  ;; Plan A: if line has an unfinished paren-group, go to end-of-group
	  (while (= -1 (nth 0 (parse-partial-sexp (point) tmp-end -1)))
	    (setq top (point)))		; Get the outermost parens in line
	  (goto-char top)
	  (while (< (point) tmp-end)
	    (parse-partial-sexp (point) tmp-end nil t) ; To start-sexp or eol
	    (or (eolp) (forward-sexp 1)))
	  (if (> (point) tmp-end)	; Check for an unfinished block
	      nil
	    (if (eq ?\) (preceding-char))
		;; closing parens can be preceded by up to three sexps
		(progn ;; Plan B: find by REGEXP block followup this line
		  (setq top (point))
		  (condition-case nil
		      (progn
			(forward-sexp -2)
			(if (eq (following-char) ?$ ) ; for my $var (list)
			    (progn
			      (forward-sexp -1)
			      (if (looking-at "\\(state\\|my\\|local\\|our\\|field\\)\\>")
				  (forward-sexp -1))))
			(if (looking-at
			     (concat "\\(elsif\\|if\\|unless\\|while\\|until"
                                     "\\|try\\|catch\\|finally\\|defer"
				     "\\|for\\(each\\)?\\>\\(\\("
				     cperl-maybe-white-and-comment-rex
				     "\\(state\\|my\\|local\\|our\\|field\\)\\)?"
				     cperl-maybe-white-and-comment-rex
                                     (rx (eval cperl--basic-variable-rx))
				     "\\)?\\)\\>"))
			    (progn
			      (goto-char top)
			      (forward-sexp 1)
			      (setq top (point)))
			  ;; no block to be processed: expression ends here
			  (setq done t)))
		    (error (setq done t)))
		  (goto-char top))
	      (if (looking-at		; Try Plan C: continuation block
		   (concat cperl-maybe-white-and-comment-rex
			   "\\<\\(else\\|elsif\\|continue\\)\\>"))
		  (progn
		    (goto-char (match-end 0))
                    (setq tmp-end (line-end-position)))
		(setq done t))))
          (setq tmp-end (line-end-position)))
	(goto-char tmp-end)
	(setq tmp-end (point-marker)))
      (if cperl-indent-region-fix-constructs
	  (cperl-fix-line-spacing tmp-end))
      (cperl-indent-region (point) tmp-end))))

(defun cperl-fix-line-spacing (&optional end parse-data)
  "Improve whitespace in a conditional/loop construct.
Returns some position at the last line.
Process until END, use PARSE-DATA from a previous call."
  (interactive)
  (or end
      (setq end (point-max)))
  (let ((ee (line-end-position))
	(cperl-indent-region-fix-constructs
	 (or cperl-indent-region-fix-constructs 1))
	p pp ml have-brace ret)
    (save-excursion
      (beginning-of-line)
      (setq ret (point))
      ;;  }? continue
      ;;  blah; }
      (if (not
	   (or (looking-at "[ \t]*\\(els\\(e\\|if\\)\\|continue\\|if\\|while\\|for\\(each\\)?\\|unless\\|until\\)\\_>")
	       (setq have-brace (save-excursion (search-forward "}" ee t)))))
	  nil				; Do not need to do anything
	;; Looking at:
	;; }
	;; else
	(if cperl-merge-trailing-else
	    (if (looking-at
		 "[ \t]*}[ \t]*\n[ \t\n]*\\(els\\(e\\|if\\)\\|continue\\)\\_>")
		(progn
		  (search-forward "}")
		  (setq p (point))
		  (skip-chars-forward " \t\n")
		  (delete-region p (point))
	      (insert (make-string cperl-indent-region-fix-constructs ?\s))
		  (beginning-of-line)))
	  (if (looking-at "[ \t]*}[ \t]*\\(els\\(e\\|if\\)\\|continue\\)\\_>")
	      (save-excursion
		  (search-forward "}")
		  (delete-horizontal-space)
		  (insert "\n")
		  (setq ret (point))
		  (if (cperl-indent-line parse-data)
		      (progn
			(cperl-fix-line-spacing end parse-data)
			(setq ret (point)))))))
	;; Looking at:
	;; }     else
	(if (looking-at "[ \t]*}\\(\t*\\|[ \t][ \t]+\\)\\<\\(els\\(e\\|if\\)\\|continue\\)\\_>")
	    (progn
	      (search-forward "}")
	      (delete-horizontal-space)
	      (insert (make-string cperl-indent-region-fix-constructs ?\s))
	      (beginning-of-line)))
	;; Looking at:
	;; else   {
	(if (looking-at
	     "[ \t]*}?[ \t]*\\<\\(els\\(e\\|if\\)\\|continue\\|unless\\|if\\|while\\|for\\(each\\)?\\|until\\)\\>\\(\t*\\|[ \t][ \t]+\\)[^ \t\n#]")
	    (progn
	      (forward-word-strictly 1)
	      (delete-horizontal-space)
	      (insert (make-string cperl-indent-region-fix-constructs ?\s))
	      (beginning-of-line)))
	;; Looking at:
	;; foreach my    $var
	(if (looking-at
	     "[ \t]*\\<for\\(each\\)?[ \t]+\\(state\\|my\\|local\\|our\\)\\(\t*\\|[ \t][ \t]+\\)[^ \t\n]")
	    (progn
	      (forward-word-strictly 2)
	      (delete-horizontal-space)
	      (insert (make-string cperl-indent-region-fix-constructs ?\s))
	      (beginning-of-line)))
	;; Looking at:
	;; foreach my $var     (
	(if (looking-at
             (rx (sequence (0+ blank) symbol-start
                           "for" (opt "each")
                           (1+ blank)
                           (or "state" "my" "local" "our")
                           (0+ blank)
                           "$" (eval cperl--basic-identifier-rx)
                           (1+ blank)
                           (not (in " \t\n#")))))
	    (progn
	      (forward-sexp 3)
	      (delete-horizontal-space)
	      (insert
	       (make-string cperl-indent-region-fix-constructs ?\s))
	      (beginning-of-line)))
	;; Looking at (with or without "}" at start, ending after "({"):
	;; } foreach my $var ()         OR   {
	(if (looking-at
             (rx (sequence
                  (0+ blank)
                  (opt (sequence "}" (0+ blank) ))
                  symbol-start
                  (or "else" "elsif" "continue" "if" "unless" "while" "until"
                      "try" "catch" "finally" "defer"
                      (sequence (or "for" "foreach")
                                (opt
                                 (opt (sequence (1+ blank)
                                                (or "state" "my" "local" "our")))
                                 (0+ blank)
                                 "$" (eval cperl--basic-identifier-rx))))
                  symbol-end
                  (group-n 1
                           (or
                            (or (sequence (0+ blank) "(")
                                (sequence (eval cperl--ws*-rx) "{"))
                            (sequence (0+ blank) "{"))))))
	    (progn
	      (setq ml (match-beginning 1)) ; "(" or "{" after control word
	      (re-search-forward "[({]")
	      (forward-char -1)
	      (setq p (point))
	      (if (eq (following-char) ?\( )
		  (progn
		    (forward-sexp 1)
		    (setq pp (point)))	; past parenthesis-group
		;; after `else' or nothing
		(if ml			; after `else'
		    (skip-chars-backward " \t\n")
		  (beginning-of-line))
		(setq pp nil))
	      ;; Now after the sexp before the brace
	      ;; Multiline expr should be special
	      (setq ml (and pp (save-excursion (goto-char p)
					       (search-forward "\n" pp t))))
	      (if (and (or (not pp) (< pp end))	; Do not go too far...
		       (looking-at "[ \t\n]*{"))
		  (progn
		    (cond
		     ((bolp)		; Were before `{', no if/else/etc
		      nil)
		     ((looking-at "\\(\t*\\| [ \t]+\\){") ; Not exactly 1 SPACE
		      (delete-horizontal-space)
		      (if (if ml
			      cperl-extra-newline-before-brace-multiline
			    cperl-extra-newline-before-brace)
			  (progn
			    (delete-horizontal-space)
			    (insert "\n")
			    (setq ret (point))
			    (if (cperl-indent-line parse-data)
				(progn
				  (cperl-fix-line-spacing end parse-data)
				  (setq ret (point)))))
			(insert
			 (make-string cperl-indent-region-fix-constructs ?\s))))
		     ((and (looking-at "[ \t]*\n")
			   (not (if ml
				    cperl-extra-newline-before-brace-multiline
				  cperl-extra-newline-before-brace)))
		      (setq pp (point))
		      (skip-chars-forward " \t\n")
		      (delete-region pp (point))
		      (insert
		       (make-string cperl-indent-region-fix-constructs ?\ )))
		     ((and (looking-at "[\t ]*{")
			   (if ml cperl-extra-newline-before-brace-multiline
			     cperl-extra-newline-before-brace))
		      (delete-horizontal-space)
		      (insert "\n")
		      (setq ret (point))
		      (if (cperl-indent-line parse-data)
			  (progn
			    (cperl-fix-line-spacing end parse-data)
			    (setq ret (point))))))
		    ;; Now we are before `{'
		    (if (looking-at "[ \t\n]*{[ \t]*[^ \t\n#]")
			(progn
			  (skip-chars-forward " \t\n")
			  (setq pp (point))
			  (forward-sexp 1)
			  (setq p (point))
			  (goto-char pp)
			  (setq ml (search-forward "\n" p t))
			  (if (or cperl-break-one-line-blocks-when-indent ml)
			      ;; not good: multi-line BLOCK
			      (progn
				(goto-char (1+ pp))
				(delete-horizontal-space)
				(insert "\n")
				(setq ret (point))
				(if (cperl-indent-line parse-data)
				    (setq ret (cperl-fix-line-spacing end parse-data)))))))))))
	(beginning-of-line)
        (setq p (point) pp (line-end-position)) ; May be different from ee.
	;; Now check whether there is a hanging `}'
	;; Looking at:
	;; } blah
	(if (and
	     cperl-fix-hanging-brace-when-indent
	     have-brace
	     (not (looking-at "[ \t]*}[ \t]*\\(\\<\\(els\\(if\\|e\\)\\|continue\\|while\\|until\\)\\>\\|$\\|#\\)"))
	     (condition-case nil
		 (progn
		   (up-list 1)
		   (if (and (<= (point) pp)
			    (eq (preceding-char) ?\} )
			    (cperl-after-block-and-statement-beg (point-min)))
		       t
		     (goto-char p)
		     nil))
	       (error nil)))
	    (progn
	      (forward-char -1)
	      (skip-chars-backward " \t")
	      (if (bolp)
		  ;; `}' was the first thing on the line, insert NL *after* it.
		  (progn
		    (cperl-indent-line parse-data)
		    (search-forward "}")
		    (delete-horizontal-space)
		    (insert "\n"))
		(delete-horizontal-space)
		(or (eq (preceding-char) ?\;)
		    (bolp)
		    (and (eq (preceding-char) ?\} )
			 (cperl-after-block-p (point-min)))
		    (insert ";"))
		(insert "\n")
		(setq ret (point)))
	      (if (cperl-indent-line parse-data)
		  (setq ret (cperl-fix-line-spacing end parse-data)))
	      (beginning-of-line)))))
    ret))

(defvar cperl-update-start)		; Do not need to make them local
(defvar cperl-update-end)
(defun cperl-delay-update-hook (beg end _old-len)
  "Process reformatting between BEG and END in `after-change-functions'."
  (setq cperl-update-start (min beg (or cperl-update-start (point-max))))
  (setq cperl-update-end (max end (or cperl-update-end (point-min)))))

(defun cperl-indent-region (start end)
  "Simple variant of indentation of region in CPerl mode.
Should be slow.  Will not indent comment if it starts at `comment-indent'
or looks like continuation of the comment on the previous line.
Indents all the lines whose first character is between START and END
inclusive.

If `cperl-indent-region-fix-constructs', will improve spacing on
conditional/loop constructs."
  (interactive "r")
  (cperl-update-syntaxification end)
  (save-excursion
    (let (cperl-update-start cperl-update-end (h-a-c after-change-functions))
      (let ((indent-info (list nil nil nil)	; Cannot use '(), since will modify
			 )
	    after-change-functions	; Speed it up!
	    comm old-comm-indent new-comm-indent i empty)
	(if h-a-c (add-hook 'after-change-functions #'cperl-delay-update-hook))
	(goto-char start)
	(setq old-comm-indent (and (cperl-to-comment-or-eol)
				   (current-column))
	      new-comm-indent old-comm-indent)
	(goto-char start)
	(setq end (set-marker (make-marker) end)) ; indentation changes pos
	(or (bolp) (beginning-of-line 2))
	(while (and (<= (point) end) (not (eobp))) ; bol to check start
	  (if (or
	       (setq empty (looking-at "[ \t]*\n"))
	       (and (setq comm (looking-at "[ \t]*#"))
		    (or (eq (current-indentation) (or old-comm-indent
						      comment-column))
			(setq old-comm-indent nil))))
	      (if (and old-comm-indent
		       (not empty)
		       (= (current-indentation) old-comm-indent)
		       (not (eq (get-text-property (point) 'syntax-type) 'pod))
		       (not (eq (get-text-property (point) 'syntax-table)
				cperl-st-cfence)))
		  (let ((comment-column new-comm-indent))
		    (indent-for-comment)))
	    (progn
              ;; FIXME: It would be nice to keep indent-info, but this
              ;; doesn not work if the region contains continuation
              ;; lines (see `cperl-calculate-indent') -- haj 2023-06-30
              (setq indent-info (list nil nil nil))
	      (setq i (cperl-indent-line indent-info))
	      (or comm
		  (not i)
		  (progn
		    (if cperl-indent-region-fix-constructs
			(goto-char (cperl-fix-line-spacing end indent-info)))
		    (if (setq old-comm-indent
			      (and (cperl-to-comment-or-eol)
				   (not (memq (get-text-property (point)
								 'syntax-type)
					      '(pod here-doc)))
				   (not (eq (get-text-property (point)
							       'syntax-table)
					    cperl-st-cfence))
				   (current-column)))
			(progn (indent-for-comment)
			       (skip-chars-backward " \t")
			       (skip-chars-backward "#")
			       (setq new-comm-indent (current-column))))))))
	  (beginning-of-line 2)))
      ;; Now run the update hooks
      (and after-change-functions
	   cperl-update-end
	   (save-excursion
	     (goto-char cperl-update-end)
	     (insert " ")
	     (delete-char -1)
	     (goto-char cperl-update-start)
	     (insert " ")
	     (delete-char -1))))))

;; Stolen from lisp-mode with a lot of improvements

(defun cperl-fill-paragraph (&optional justify iteration)
  "Like `fill-paragraph', but handle CPerl comments.
If any of the current line is a comment, fill the comment or the
block of it that point is in, preserving the comment's initial
indentation and initial hashes.  Behaves usually outside of comment.
JUSTIFY is passed to `fill-paragraph'.  ITERATION is for internal
use, it indicates a recursive call."
  ;; (interactive "P") ; Only works when called from fill-paragraph.  -stef
  (let (;; Non-nil if the current line contains a comment.
	has-comment
	fill-paragraph-function		; do not recurse
	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	;; Line that contains code and comment (or nil)
	start
	c spaces len dc (comment-column comment-column))
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond

       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))

       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ((cperl-to-comment-or-eol)
	(setq has-comment t)
	(looking-at "#+[ \t]*")
	(setq start (point) c (current-column)
	      comment-fill-prefix
	      (concat (make-string (current-column) ?\s)
		      (buffer-substring (match-beginning 0) (match-end 0)))
	      spaces (progn (skip-chars-backward " \t")
			    (buffer-substring (point) start))
	      dc (- c (current-column)) len (- start (point))
	      start (point-marker))
	(delete-char len)
	(insert (make-string dc ?-)))))	; Placeholder (to avoid splitting???)
    (if (not has-comment)
	(fill-paragraph justify)       ; Do the usual thing outside of comment
      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
	(narrow-to-region
	 ;; Find the first line we should include in the region to fill.
	 (if start (progn (beginning-of-line) (point))
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*#+[ \t]*[^ \t\n#]")))
	     ;; We may have gone to far.  Go forward again.
	     (or (looking-at "^[ \t]*#+[ \t]*[^ \t\n#]")
		 (forward-line 1))
	     (point)))
	 ;; Find the beginning of the first line past the region to fill.
	 (save-excursion
	   (while (progn (forward-line 1)
			 (looking-at "^[ \t]*#+[ \t]*[^ \t\n#]")))
	   (point)))
	;; Remove existing hashes
	(goto-char (point-min))
	(save-excursion
	  (while (progn (forward-line 1) (< (point) (point-max)))
	    (skip-chars-forward " \t")
	    (if (looking-at "#+")
		(progn
		  (if (and (eq (point) (match-beginning 0))
			   (not (eq (point) (match-end 0)))) nil
		    (error
 "Bug in Emacs: `looking-at' in `narrow-to-region': match-data is garbage"))
		(delete-char (- (match-end 0) (match-beginning 0)))))))

	;; Lines with only hashes on them can be paragraph boundaries.
	(let ((paragraph-start (concat paragraph-start "\\|^[ \t#]*$"))
	      (paragraph-separate (concat paragraph-start "\\|^[ \t#]*$"))
	      (fill-prefix comment-fill-prefix))
	  (fill-paragraph justify)))
      (if (and start)
	  (progn
	    (goto-char start)
	    (if (> dc 0)
		(progn (delete-char dc) (insert spaces)))
	    (if (or (= (current-column) c) iteration) nil
	      (setq comment-column c)
	      (indent-for-comment)
	      ;; Repeat once more, flagging as iteration
	      (cperl-fill-paragraph justify t))))))
  t)

(defun cperl-do-auto-fill ()
  "Break out if the line is short enough."
  (if (> (save-excursion
	   (end-of-line)
	   (current-column))
	 fill-column)
      (let ((c (save-excursion (beginning-of-line)
			       (cperl-to-comment-or-eol) (point)))
	    (s (memq (following-char) '(?\s ?\t))) marker)
	(if (>= c (point))
	    ;; Don't break line inside code: only inside comment.
	    nil
	  (setq marker (point-marker))
	  (fill-paragraph nil)
	  (goto-char marker)
	  ;; Is not enough, sometimes marker is a start of line
	  (if (bolp) (progn (re-search-forward "#+[ \t]*")
			    (goto-char (match-end 0))))
	  ;; Following space could have gone:
	  (if (or (not s) (memq (following-char) '(?\s ?\t))) nil
	    (insert " ")
	    (backward-char 1))
	  ;; Previous space could have gone:
	  (or (memq (preceding-char) '(?\s ?\t)) (insert " "))))))

;; The following lists are used for categorizing the entries found by
;; `cperl-imenu--create-perl-index'.
(defvar cperl-imenu-package-keywords '("package" "class" "role"))
(defvar cperl-imenu-sub-keywords '("sub" "method" "function" "fun"))
(defvar cperl-imenu-pod-keywords '("=head"))

(defun cperl-imenu--create-perl-index ()
  "Implement `imenu-create-index-function' for CPerl mode.
This function relies on syntaxification to exclude lines which
look like declarations but actually are part of a string, a
comment, or POD."
  (interactive) ; We'll remove that at some point
  (goto-char (point-min))
  (cperl-update-syntaxification (point-max))
  (let ((case-fold-search nil)
	(index-alist '())
	(index-package-alist '())
	(index-pod-alist '())
	(index-sub-alist '())
	(index-unsorted-alist '())
	(namespace-stack '())           ; for package NAME BLOCK
	(current-namespace "(main)")
	(current-namespace-end (point-max))) ; end of package scope
    ;; collect index entries
    (while (re-search-forward (rx (eval cperl--imenu-entries-rx)) nil t)
      ;; First, check whether we have left the scope of previously
      ;; recorded packages, and if so, eliminate them from the stack.
      (while (< current-namespace-end (point))
	(setq current-namespace (pop namespace-stack))
	(setq current-namespace-end (pop namespace-stack)))
      (let ((state (syntax-ppss))
            (entry-type (match-string 1))
	    name marker) ; for the "current" entry
	(cond
	 ((nth 3 state) nil)            ; matched in a string, so skip
         ((member entry-type cperl-imenu-package-keywords) ; package or class
	  (unless (nth 4 state)         ; skip if in a comment
	    (setq name (match-string-no-properties 2)
		  marker (copy-marker (match-end 2)))
	    (if  (string= (match-string 3) ";")
		(setq current-namespace name)  ; package NAME;
	      ;; No semicolon, therefore we have: package NAME BLOCK.
	      ;; Stash the current package, because we need to restore
	      ;; it after the end of BLOCK.
	      (push current-namespace-end namespace-stack)
	      (push current-namespace namespace-stack)
	      ;; record the current name and its scope
	      (setq current-namespace name)
	      (setq current-namespace-end (save-excursion
					  (goto-char (match-beginning 3))
					  (forward-sexp)
					  (point))))
	    (push (cons name marker) index-package-alist)
	    (push (cons (concat entry-type " " name) marker) index-unsorted-alist)))
	 ((or (member entry-type cperl-imenu-sub-keywords) ; sub or method
              (string-equal entry-type ""))                ; named blocks
	  (unless (nth 4 state)         ; skip if in a comment
	    (setq name (match-string-no-properties 2)
		  marker (copy-marker (match-end 2)))
	    ;; Qualify the sub name with the namespace if it doesn't
	    ;; already have one, and if it isn't lexically scoped.
	    ;; "my" and "state" subs are lexically scoped, but "our"
	    ;; are just lexical aliases to package subs.
	    (if (and (null (string-match "::" name))
		     (or (null (match-string 3))
			 (string-equal (match-string 3) "our")))
	      (setq name (concat current-namespace "::" name)))
	    (let ((index (cons name marker)))
	      (push index index-alist)
	      (push index index-sub-alist)
	      (push index index-unsorted-alist))))
         ((string-equal entry-type "field")
	  (unless (nth 4 state)         ; skip if in a comment
            (let ((limit (match-end 0))
                  field-name)
              (save-excursion
                (goto-char (match-end 1))
                (re-search-forward
                 (rx (in "$%@")
                     (group-n 1 (eval cperl--basic-identifier-rx)))
                 limit t)
                (setq field-name (match-string-no-properties 1))
                (when field-name
                  (while (re-search-forward
                          (rx (group-n 1 (eval cperl--basic-identifier-rx))
                              (optional
                               (sequence
                                "("
                                (group-n 2
                                  (eval cperl--basic-identifier-rx))
                                ")")))
                          limit t)
                    (when (member (match-string 1) '("reader" "writer"))
                      (let* ((name (or (match-string 2)
                                       (if (string-equal
                                            (match-string 1) "reader")
                                           field-name)
                                       (concat "set_" field-name)))
                             (qname (concat current-namespace "->" name))
                             (index (cons qname (match-beginning 1))))
	                (push index index-alist)
	                (push index index-sub-alist)
	                (push index index-unsorted-alist)))))))))
	 ((member entry-type cperl-imenu-pod-keywords)  ; POD heading
	  (when (get-text-property (match-beginning 2) 'in-pod)
	    (setq name (concat (make-string
				(* 3 (- (char-after (match-beginning 3)) ?1))
				?\ )
			       (match-string-no-properties 2))
		  marker (copy-marker (match-beginning 2)))
	    (push (cons name marker) index-pod-alist)
	    (push (cons (concat "=" name) marker) index-unsorted-alist)))
	 (t (error "Unidentified match: %s" (match-string 0))))))
    ;; Now format the collected stuff
    (setq index-alist
	  (if (default-value 'imenu-sort-function)
	      (sort index-alist (default-value 'imenu-sort-function))
	    (nreverse index-alist)))
    (and index-pod-alist
	 (push (cons "+POD headers+..."
		     (nreverse index-pod-alist))
	       index-alist))
    (and (or index-package-alist index-sub-alist)
	 (let ((lst index-package-alist) hier-list pack elt group name)
	   ;; reverse and uniquify.
	   (while lst
	     (setq elt (car lst) lst (cdr lst) name (car elt))
	     (if (assoc name hier-list) nil
	       (setq hier-list (cons (cons name (cdr elt)) hier-list))))
	   (setq lst index-sub-alist)
	   (while lst
	     (setq elt (car lst) lst (cdr lst))
	     (cond ((string-match
                     (rx (sequence (or "::" "'")
                                   (eval cperl--basic-identifier-rx)
                                   string-end))
                     (car elt))
		    (setq pack (substring (car elt) 0 (match-beginning 0)))
		    (if (setq group (assoc pack hier-list))
			(if (listp (cdr group))
			    ;; Have some functions already
			    (setcdr group
				    (cons (cons (substring
						 (car elt)
						 (+ 2 (match-beginning 0)))
						(cdr elt))
					  (cdr group)))
			  (setcdr group (list (cons (substring
						     (car elt)
						     (+ 2 (match-beginning 0)))
						    (cdr elt)))))
		      (setq hier-list
			    (cons (cons pack
					(list (cons (substring
						     (car elt)
						     (+ 2 (match-beginning 0)))
						    (cdr elt))))
				  hier-list))))))
	   (push (cons "+Hierarchy+..."
		       hier-list)
		 index-alist)))
    (and index-package-alist
	 (push (cons "+Classes,Packages+..."
		     (nreverse index-package-alist))
	       index-alist))
    (and (or index-package-alist index-pod-alist
	     (default-value 'imenu-sort-function))
	 index-unsorted-alist
	 (push (cons "+Unsorted List+..."
		     (nreverse index-unsorted-alist))
	       index-alist))
    ;; Finally, return the whole collection
    index-alist))


;; Suggested by Mark A. Hershberger
(defun cperl-outline-level ()
  "Guess the outline level.  0 for a package, 1 for a sub.
In POD, returns the level of the current heading."
  (looking-at outline-regexp)
  (cond ((not (match-beginning 1)) 0)	; beginning-of-file
        ;; 2=package-group, 5=package-name 8=sub-name 16=head-level
	((match-beginning 2) 0)		; package
	((match-beginning 8) 1)		; sub
	((match-beginning 16)
	 (- (char-after (match-beginning 16)) ?0)) ; headN ==> N
	(t 5)))				; should not happen


(defun cperl-windowed-init ()
  "Initialization under windowed version."
  (cond ((featurep 'ps-print)
	 (or cperl-faces-init
	     (progn
	       (cperl-init-faces))))
	((not cperl-faces-init)
	 (add-hook 'font-lock-mode-hook
                   (lambda ()
                     (if (memq major-mode '(perl-mode cperl-mode))
                         (progn
                           (or cperl-faces-init (cperl-init-faces))))))
	 (eval-after-load
	     "ps-print"
	   '(or cperl-faces-init (cperl-init-faces))))))

(defvar cperl-font-lock-keywords-1 nil
  "Additional expressions to highlight in Perl mode.  Minimal set.")
(defvar cperl-font-lock-keywords nil
  "Additional expressions to highlight in Perl mode.  Default set.")
(defvar cperl-font-lock-keywords-2 nil
  "Additional expressions to highlight in Perl mode.  Maximal set.")

(defun cperl-load-font-lock-keywords ()
  "Initialize the default set of cperl faces."
  (or cperl-faces-init (cperl-init-faces))
  cperl-font-lock-keywords)

(defun cperl-load-font-lock-keywords-1 ()
  "Initialize the minimal set of cperl faces."
  (or cperl-faces-init (cperl-init-faces))
  cperl-font-lock-keywords-1)

(defun cperl-load-font-lock-keywords-2 ()
  "Initialize the maximal set of cperl faces."
  (or cperl-faces-init (cperl-init-faces))
  cperl-font-lock-keywords-2)

(defun cperl-font-lock-syntactic-face-function (state)
  "Apply faces according to their syntax type, given as STATE.
In CPerl mode, this is used for here-documents which have been
marked as c-style comments.  For everything else, delegate to the
default function."
  (cond
   ;; A c-style comment is a HERE-document.  Fontify if requested.
   ((and (eq 2 (nth 7 state))
         cperl-pod-here-fontify)
    cperl-here-face)
   (t (funcall (default-value 'font-lock-syntactic-face-function) state))))

(defface cperl-method-call
  '((t (:inherit default )))
  "Font Lock mode face for method calls.
Usually, method calls are not fontified.
We use this face to prevent calls to methods which look like
builtin functions to be fontified like, well, builtin
functions (which they are not).  Inherits from `default'.")

(defun cperl-init-faces ()
  "Initialize the faces for CPerl mode."
  (condition-case errs
      (progn
	(let (t-font-lock-keywords t-font-lock-keywords-1)
	  (setq
	   t-font-lock-keywords
	   (list
            ;; -------- function definition _and_ declaration
            ;; (matcher (subexp facespec))
            ;; facespec is evaluated depending on whether the
            ;; statement ends in a "{" (definition) or ";"
            ;; (declaration without body)
	    (list (concat "\\<" cperl-sub-regexp
                          ;; group 1: optional subroutine name
                          (rx
                           (sequence (eval cperl--ws+-rx)
                                     (group (optional
                                             (eval cperl--normal-identifier-rx)))))
                          ;; "fontified" elsewhere: Prototype
                          (rx (optional
                               (sequence (eval cperl--ws*-rx)
                                         (eval cperl--prototype-rx))))
                          ;; fontified elsewhere: Attributes
                          (rx (optional (sequence (eval cperl--ws*-rx)
                                                  (eval cperl--attribute-list-rx))))
                          (rx (eval cperl--ws*-rx))
                          ;; group 2: Identifies the start of the anchor
                          (rx (group
                               (or (group-n 3 ";") ; Either a declaration...
                                   "{"             ; ... or a code block
                                   ;; ... or a complete signature
                                   (sequence (eval cperl--signature-rx)
                                             (eval cperl--ws*-rx))
                                   ;; ... or the start of a "sloppy" signature
                                   (sequence (eval cperl--sloppy-signature-rx))
                                   ;; make sure we have a reasonably
                                   ;; short match for an incomplete sub
                                   (not (in ";{("))
                                   buffer-end))))
		  '(1 (if (match-beginning 3)
			  'font-lock-variable-name-face
		        'font-lock-function-name-face)
                      nil ; override
                      t)  ; laxmatch in case of anonymous subroutines
                  ;; -------- anchored: Signature
                  `(,(rx (sequence (in "(,")
                                   (eval cperl--ws*-rx)
                                   (group (eval cperl--basic-variable-rx))))
                    (progn
                      (goto-char (match-beginning 2)) ; pre-match: Back to sig
                      ;; While typing, forward-sexp might fail with a scan error.
                      ;; If so, stop looking for declarations at (match-end 2)
                      (condition-case nil
                          (save-excursion
                            (forward-sexp)
                            (point))
                        (error (match-end 2))))
                    nil
                    (1 font-lock-variable-name-face)))
            ;; -------- flow control
            ;; (matcher . subexp) font-lock-keyword-face by default
	    ;; This highlights declarations and definitions differently.
	    ;; We do not try to highlight in the case of attributes:
	    ;; it is already done by `cperl-find-pods-heres'
	    (cons
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
              (regexp-opt
	       (append
                cperl-sub-keywords
                '("if" "until" "while" "elsif" "else"
                  "given" "when" "default" "break"
                  "unless" "for"
                  "try" "catch" "defer" "finally"
                  "foreach" "continue" "exit" "die" "last" "goto" "next"
                  "redo" "return" "local" "exec"
                  "do" "dump"
                  "use" "our"
                  "require" "package" "eval" "evalbytes" "my" "state"
                  "class" "field" "method"
                  "ADJUST" "BEGIN" "CHECK"
                  "END" "INIT" "UNITCHECK"
                  ;; not in core, but per popular request
                  "async" "await")))    ; Flow control
	      "\\)\\>") 2)		; was "\\)[ \n\t;():,|&]"
					; In what follows we use `type' style
					; for overwritable builtins
            ;; -------- avoid method calls being fontified as keywords
            ;; (matcher (subexp facespec))
            (list
             (rx "->" (* space) (group-n 1(eval cperl--basic-identifier-rx)))
             1 ''cperl-method-call)
            ;; -------- builtin functions
            ;; (matcher subexp facespec)
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
              (regexp-opt
               '("CORE" "__FILE__" "__LINE__" "__SUB__" "__PACKAGE__" "__CLASS__"
                 "abs" "accept" "alarm" "and" "atan2"
                 "bind" "binmode" "bless" "caller"
                 "chdir" "chmod" "chown" "chr" "chroot" "close"
                 "closedir" "cmp" "connect" "continue" "cos" "crypt"
                 "dbmclose" "dbmopen" "die" "dump" "endgrent"
                 "endhostent" "endnetent" "endprotoent" "endpwent"
                 "endservent" "eof" "eq" "exec" "exit" "exp" "fc" "fcntl"
                 "fileno" "flock" "fork" "formline" "ge" "getc"
                 "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
                 "gethostbyname" "gethostent" "getlogin"
                 "getnetbyaddr" "getnetbyname" "getnetent"
                 "getpeername" "getpgrp" "getppid" "getpriority"
                 "getprotobyname" "getprotobynumber" "getprotoent"
                 "getpwent" "getpwnam" "getpwuid" "getservbyname"
                 "getservbyport" "getservent" "getsockname"
                 "getsockopt" "glob" "gmtime" "gt" "hex" "index" "int"
                 "ioctl" "join" "kill" "lc" "lcfirst" "le" "length"
                 "link" "listen" "localtime" "lock" "log" "lstat" "lt"
                 "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "ne"
                 "not" "oct" "open" "opendir" "or" "ord" "pack" "pipe"
                 "quotemeta" "rand" "read" "readdir" "readline"
                 "readlink" "readpipe" "recv" "ref" "rename" "require"
                 "reset" "reverse" "rewinddir" "rindex" "rmdir" "seek"
                 "seekdir" "select" "semctl" "semget" "semop" "send"
                 "setgrent" "sethostent" "setnetent" "setpgrp"
                 "setpriority" "setprotoent" "setpwent" "setservent"
                 "setsockopt" "shmctl" "shmget" "shmread" "shmwrite"
                 "shutdown" "sin" "sleep" "socket" "socketpair"
                 "sprintf" "sqrt" "srand" "stat" "substr" "symlink"
                 "syscall" "sysopen" "sysread" "sysseek" "system" "syswrite" "tell"
                 "telldir" "time" "times" "truncate" "uc" "ucfirst"
                 "umask" "unlink" "unpack" "utime" "values" "vec"
                 "wait" "waitpid" "wantarray" "warn" "write" "x" "xor"))
              "\\)\\>")
             2 'font-lock-type-face)
	    ;; In what follows we use `other' style
	    ;; for nonoverwritable builtins
            ;; This is a bit shaky because the status
            ;; "nonoverwritable" can change between Perl versions.
            ;; -------- "non overridable" functions
            ;; (matcher subexp facespec)
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
              (regexp-opt
               '("AUTOLOAD" "BEGIN" "CHECK" "DESTROY" "END" "INIT" "UNITCHECK"
                 "__END__" "__DATA__"
                 "all" "any"
                 "break" "catch" "chomp" "chop" "default"
                 "defined" "delete" "do" "each" "else" "elsif" "eval"
                 "evalbytes" "exists" "finally" "for" "foreach" "format" "given"
                 "goto" "grep" "if" "keys" "last" "local" "m" "map" "my" "next"
                 "no" "our" "package" "pop" "pos" "print" "printf" "prototype"
                 "push" "q" "qq" "qr" "qw" "qx" "redo" "return" "s" "say" "scalar"
                 "shift" "sort" "splice" "split" "state" "study" "sub" "tie"
                 "tied" "tr" "try" "undef" "unless" "unshift" "untie" "until"
                 "use" "when" "while" "y"))
              "\\)\\>")
	     2 ''cperl-nonoverridable-face) ; unbound as var, so: doubly quoted
	    ;;		(mapconcat #'identity
	    ;;			   '("#endif" "#else" "#ifdef" "#ifndef" "#if"
	    ;;			     "#include" "#define" "#undef")
	    ;;			   "\\|")
            ;; -------- -X file tests
            ;; (matcher subexp facespec)
	    '("-[rwxoRWXOezsfdlpSbctugkTBMAC]\\>\\([ \t]+_\\>\\)?" 0
	      font-lock-function-name-face keep) ; Not very good, triggers at "[a-z]"
            ;; -------- various stuff calling for a package name
            ;; (matcher (subexp facespec) (subexp facespec))
            `(,(rx (sequence
                    (or (sequence (or line-start space "{" )
                                  (or "package" "require" "use" "import"
                                      "no" "bootstrap" "class")
                                  (eval cperl--ws+-rx))
                        (sequence (group-n 2 (sequence ":"
                                                       (eval cperl--ws*-rx)
                                                       "isa"))
                                  "("
                                  (eval cperl--ws*-rx)))
                    (group-n 1 (eval cperl--normal-identifier-rx))
                    (any " \t\n;)"))) ; require A if B;
	      (1 font-lock-function-name-face)
              (2 font-lock-constant-face t t))
            ;; -------- formats
            ;; (matcher subexp facespec)
	    '("^[ \t]*format[ \t]+\\([a-zA-Z_][a-zA-Z_0-9:]*\\)[ \t]*=[ \t]*$"
	      1 font-lock-function-name-face)
            ;; -------- labels
            ;; (matcher subexp facespec)
            `(,(rx
                (sequence
                 (0+ space)
                 (group (eval cperl--label-rx))
                 (0+ space)
                 (or line-end "#" "{"
                     (sequence word-start
                               (or "until" "while" "for" "foreach" "do")
                               word-end))))
              1 font-lock-constant-face)
            ;; -------- labels as targets (no trailing colon!)
            ;; (matcher subexp facespec)
            `(,(rx
                (sequence
                 symbol-start
                 (or "continue" "next" "last" "redo" "break" "goto")
                 (1+ space)
                 (group (eval cperl--basic-identifier-rx))))
              1 font-lock-constant-face)
	    ;; Uncomment to get perl-mode-like vars
            ;; '("[$*]{?\\(\\sw+\\)" 1 font-lock-variable-name-face)
            ;; '("\\([@%]\\|\\$#\\)\\(\\sw+\\)"
            ;;  (2 (cons font-lock-variable-name-face '(underline))))
	    ;; 1=my_etc, 2=white? 3=(+white? 4=white? 5=var
            ;; -------- variable declarations
            ;; (matcher (subexp facespec) ...
	    `(,(rx (sequence (or "state" "my" "local" "our"))
                   (eval cperl--ws*-rx)
                   (opt (group (sequence "(" (eval cperl--ws*-rx))))
                   (group
                    (in "$@%*")
                    (or
                     (eval cperl--normal-identifier-rx)
                     (eval cperl--special-identifier-rx))
                    )
                   )
              ;; (concat "\\<\\(state\\|my\\|local\\|our\\)"
	      ;;          cperl-maybe-white-and-comment-rex
	      ;;          "\\(("
	      ;;          cperl-maybe-white-and-comment-rex
	      ;;          "\\)?\\([$@%*]\\([a-zA-Z0-9_:]+\\|[^a-zA-Z0-9_]\\)\\)")
	      (2 font-lock-variable-name-face)
              ;; ... (anchored-matcher pre-form post-form subex-highlighters)
	      (,(rx (sequence point
                              (eval cperl--ws*-rx)
                              ","
                              (eval cperl--ws*-rx)
                              (group
                               (in "$@%*")
                               (or
                                (eval cperl--normal-identifier-rx)
                                (eval cperl--special-identifier-rx))
                               )
                              )
                    )
               ;; ,(concat "\\="
	       ;;  	cperl-maybe-white-and-comment-rex
	       ;;  	","
	       ;;  	cperl-maybe-white-and-comment-rex
	       ;;  	"\\([$@%*]\\([a-zA-Z0-9_:]+\\|[^a-zA-Z0-9_]\\)\\)")
	       ;; Bug in font-lock: limit is used not only to limit
	       ;; searches, but to set the "extend window for
	       ;; facification" property.  Thus we need to minimize.
	       (if (match-beginning 1)  ; list declaration
		    (save-excursion
		      (goto-char (match-beginning 1))
		      (condition-case nil
			  (forward-sexp 1)
			(error
			 (condition-case nil
			     (forward-char 200)
			   (error nil)))) ; typeahead
		      (1- (point))) ; report limit
		  (forward-char -2)) ; disable continued expr
	       nil
	       (1 font-lock-variable-name-face)))
            ;; -------- builtin constants with and without package prefix
            ;; (matcher subexp facespec)
            `(,(rx (or space (in "=<>-"))
                   (group (optional "&")
                          (optional "builtin::")
                          (or "inf" "nan")
                          symbol-end))
              1 'font-lock-constant-face)
            ;; -------- field declarations
            `(,(rx "field"
                   (eval cperl--ws+-rx)
                   (group (eval cperl--basic-variable-rx))
                   (optional (sequence
                              (eval cperl--ws+-rx)
                              (group (eval cperl--attribute-list-rx)))))
              (1 font-lock-variable-name-face)
              ;; -------- optional attributes
              ;; (anchored-matcher pre-form post-form subex-highlighters)
              (,(rx
                 (group (optional ":" (eval cperl--ws*-rx))
                        (eval cperl--basic-identifier-rx))
                 (optional "("
                           (group (eval cperl--basic-identifier-rx))
                           ")"))
               ;; pre-form: Define range for anchored matcher
               (if (match-beginning 2)
                   (progn
                     (goto-char (match-beginning 2))
                     (match-end 2))
                 ;; If there's no attribute list in match 2, set a short
                 ;; limit to the search for the anchored matcher,
                 ;; otherwise it might interpret stuff from the
                 ;; initializer expression as attribute.
                 (1+ (point)))
               nil
               (1 font-lock-constant-face)
               (2 font-lock-string-face nil t) ; lax match, value is optional
              ))
            ;; ----- foreach my $foo (
            ;; (matcher subexp facespec)
            `(,(rx symbol-start "for" (opt "each")
                   (opt (sequence (1+ blank)
                                  (or "state" "my" "local" "our")))
                   (0+ blank)
                   (group-n 1 (sequence "$"
                                        (eval cperl--basic-identifier-rx)))
                   (0+ blank) "(")
;;	    '("\\<for\\(each\\)?\\([ \t]+\\(state\\|my\\|local\\|our\\)\\)?[ \t]*\\(\\$[a-zA-Z_][a-zA-Z_0-9]*\\)[ \t]*("
	      1 font-lock-variable-name-face)
	    ;; Avoid $!, and s!!, qq!! etc. when not fontifying syntactically
            ;; -------- ! as a negation char like $false = !$true
            ;; (matcher subexp facespec)
	    '("\\(?:^\\|[^smywqrx$]\\)\\(!\\)" 1 font-lock-negation-char-face)
            ;; -------- ^ as a negation char in character classes m/[^abc]/
            ;; (matcher subexp facespec)
	    '("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)))
	  (setq
	   t-font-lock-keywords-1
	   `(
            ;; -------- bareword hash key: $foo{bar}, $foo[1]{bar}
            ;; (matcher (subexp facespec) ...
            (,(rx (or (in "]}\\%@>*&")
                       (sequence "$" (eval cperl--normal-identifier-rx)))
                   (0+ blank) "{" (0+ blank)
                   (group-n 1 (sequence (opt "-")
                                        (eval cperl--basic-identifier-rx)))
                   (0+ blank) "}")
;;	    '("\\([]}\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
	      (1 font-lock-string-face)
              ;; -------- anchored bareword hash key: $foo{bar}{baz}
              ;; ... (anchored-matcher pre-form post-form subex-highlighters)
              (,(rx point
                    (0+ blank) "{" (0+ blank)
                    (group-n 1 (sequence (opt "-")
                                         (eval cperl--basic-identifier-rx)))
                    (0+ blank) "}")
	       ;; ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
	       nil nil
	       (1 font-lock-string-face)))
            ;; -------- hash element assignments with bareword key => value
            ;; (matcher subexp facespec)
            (,(rx (in "[ \t{,()")
                   (group-n 1 (sequence (opt "-")
                                        (eval cperl--basic-identifier-rx)))
                   (0+ blank) "=>")
              1 font-lock-string-face)
             ;; -------- @$ array dereferences, $#$ last array index
             ;; (matcher (subexp facespec) (subexp facespec))
             (,(rx (group-n 1 (or "@" "$#"))
                   (group-n 2 (sequence "$"
                                        (or (eval cperl--normal-identifier-rx)
                                            (not (in " \t\n"))))))
	     ;; ("\\(@\\|\\$#\\)\\(\\$+\\([a-zA-Z_:][a-zA-Z0-9_:]*\\|[^ \t\n]\\)\\)"
	      (1 'cperl-array-face)
	      (2 font-lock-variable-name-face))
             ;; -------- %$ hash dereferences
             ;; (matcher (subexp facespec) (subexp facespec))
             (,(rx (group-n 1 "%")
                   (group-n 2 (sequence "$"
                                        (or (eval cperl--normal-identifier-rx)
                                            (not (in " \t\n"))))))
	     ;; ("\\(%\\)\\(\\$+\\([a-zA-Z_:][a-zA-Z0-9_:]*\\|[^ \t\n]\\)\\)"
	      (1 'cperl-hash-face)
	      (2 font-lock-variable-name-face))
             ;; -------- access to array/hash elements
             ;; (matcher subexp facespec)
             ;; facespec is an expression to distinguish between arrays and hashes
             (,(rx (group-n 1 (group-n 2 (in "$@%"))
                            (eval cperl--normal-identifier-rx))
                   (0+ blank)
                   (group-n 3 (in "[{")))
;;	     ("\\(\\([$@%]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)"
	      1
	      (if (= (- (match-end 2) (match-beginning 2)) 1)
		  (if (eq (char-after (match-beginning 3)) ?{)
		      'cperl-hash-face
		    'cperl-array-face)             ; arrays and hashes
		font-lock-variable-name-face)      ; Just to put something
	      nil)                                 ; do not override previous
             ;; -------- "Pure" arrays and hashes.
             ;; (matcher subexp facespec)
             ;; facespec is an expression to distinguish between arrays and hashes
             (,(rx (group-n 1 (group-n 2 (or (in "@%") "$#"))
                            (eval cperl--normal-identifier-rx)))
              1
              ;; ("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
	      (if (eq (char-after (match-beginning 2)) ?%)
		  'cperl-hash-face
		'cperl-array-face)
	      nil)
             ;;("\\([smy]\\|tr\\)\\([^a-z_A-Z0-9]\\)\\(\\([^\n\\]*||\\)\\)\\2")
             ;; Too much noise from \s* @s[ and friends
	     ;;("\\(\\<\\([msy]\\|tr\\)[ \t]*\\([^ \t\na-zA-Z0-9_]\\)\\|\\(/\\)\\)"
	     ;;(3 font-lock-function-name-face t t)
	     ;;(4
	     ;; (if (cperl-slash-is-regexp)
	     ;;    font-lock-function-name-face 'default) nil t))
	     ))
	  (if cperl-highlight-variables-indiscriminately
	      (setq t-font-lock-keywords-1
		    (append t-font-lock-keywords-1
			    (list '("\\([$*]{?\\(?:\\sw+\\|::\\)+\\)" 1
				    font-lock-variable-name-face)))))
	  (setq cperl-font-lock-keywords-1
		(if cperl-syntaxify-by-font-lock
		    (cons 'cperl-fontify-update
			  t-font-lock-keywords)
		  t-font-lock-keywords)
		cperl-font-lock-keywords cperl-font-lock-keywords-1
		cperl-font-lock-keywords-2 (append
					   t-font-lock-keywords-1
					   cperl-font-lock-keywords-1)))
        (cperl-ps-print-init)
	(setq cperl-faces-init t))
    (error (message "cperl-init-faces (ignored): %s" errs))))


(defvar ps-bold-faces)
(defvar ps-italic-faces)
(defvar ps-underlined-faces)

(defun cperl-ps-print-init ()
  "Initialization of `ps-print' components for faces used in CPerl."
  (eval-after-load "ps-print"
    '(setq ps-bold-faces
	   ;; 			font-lock-variable-name-face
	   ;;			font-lock-constant-face
	   (append '(cperl-array-face cperl-hash-face)
		   ps-bold-faces)
	   ps-italic-faces
	   ;;			font-lock-constant-face
	   (append '(cperl-nonoverridable-face cperl-hash-face)
		   ps-italic-faces)
	   ps-underlined-faces
	   ;;	     font-lock-type-face
	   (append '(cperl-array-face cperl-hash-face underline cperl-nonoverridable-face)
		   ps-underlined-faces))))

(defvar ps-print-face-extension-alist)

(defun cperl-ps-print (&optional file)
  "Pretty-print in CPerl style.
If optional argument FILE is an empty string, prints to printer, otherwise
to the file FILE.  If FILE is nil, prompts for a file name.

Style of printout regulated by the variable `cperl-ps-print-face-properties'."
  (interactive)
  (or file
      (setq file (read-from-minibuffer
		  "Print to file (if empty - to printer): "
		  (concat (buffer-file-name) ".ps")
		  nil nil 'file-name-history)))
  (or (> (length file) 0)
      (setq file nil))
  (require 'ps-print)			; To get ps-print-face-extension-alist
  (let ((ps-print-color-p t)
	(ps-print-face-extension-alist ps-print-face-extension-alist))
    (ps-extend-face-list cperl-ps-print-face-properties)
    (ps-print-buffer-with-faces file)))

(cperl-windowed-init)

(defconst cperl-styles-entries
  '(cperl-indent-level cperl-brace-offset cperl-continued-brace-offset
    cperl-label-offset cperl-extra-newline-before-brace
    cperl-extra-newline-before-brace-multiline
    cperl-merge-trailing-else
    cperl-continued-statement-offset))

(defconst cperl-style-examples
"##### Numbers etc are: cperl-indent-level cperl-brace-offset
##### cperl-continued-brace-offset cperl-label-offset
##### cperl-continued-statement-offset
##### cperl-merge-trailing-else cperl-extra-newline-before-brace

########### (Do not forget cperl-extra-newline-before-brace-multiline)

### CPerl	(=GNU - extra-newline-before-brace + merge-trailing-else) 2/0/0/-2/2/t/nil
if (foo) {
  bar
    baz;
 label:
  {
    boon;
  }
} else {
  stop;
}

### PBP (=Perl Best Practices)				4/0/0/-4/4/nil/nil
if (foo) {
    bar
	baz;
  label:
    {
	boon;
    }
}
else {
    stop;
}
### PerlStyle	(=CPerl with 4 as indent)		4/0/0/-2/4/t/nil
if (foo) {
    bar
	baz;
 label:
    {
	boon;
    }
} else {
    stop;
}

### GNU							2/0/0/-2/2/nil/t
if (foo)
  {
    bar
      baz;
  label:
    {
      boon;
    }
  }
else
  {
    stop;
  }

### C++		(=PerlStyle with braces aligned with control words) 4/0/-4/-4/4/nil/t
if (foo)
{
    bar
	baz;
 label:
    {
	boon;
    }
}
else
{
    stop;
}

### BSD		(=C++, but will not change preexisting merge-trailing-else
###		 and extra-newline-before-brace )		4/0/-4/-4/4
if (foo)
{
    bar
	baz;
 label:
    {
	boon;
    }
}
else
{
    stop;
}

### K&R		(=C++ with indent 5 - merge-trailing-else, but will not
###		 change preexisting extra-newline-before-brace)	5/0/-5/-5/5/nil
if (foo)
{
     bar
	  baz;
 label:
     {
	  boon;
     }
}
else
{
     stop;
}

### Whitesmith	(=PerlStyle, but will not change preexisting
###		 extra-newline-before-brace and merge-trailing-else) 4/0/0/-4/4
if (foo)
    {
	bar
	    baz;
    label:
	{
	    boon;
	}
    }
else
    {
	stop;
    }
"
"Examples of if/else with different indent styles (with v4.23).")

(defconst cperl-style-alist
  '(("CPerl" ;; =GNU - extra-newline-before-brace + cperl-merge-trailing-else
     (cperl-indent-level               .  2)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-continued-statement-offset .  2)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else	       .  t))

    ("PBP"  ;; Perl Best Practices by Damian Conway
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-continued-statement-offset .  4)
     (cperl-close-paren-offset         . -4)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else        .  nil)
     (cperl-indent-parens-as-block     .  t)
     (cperl-tab-always-indent          .  t))

    ("PerlStyle"			; CPerl with 4 as indent
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else	       .  t))

    ("GNU"
     (cperl-indent-level               .  2)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-continued-statement-offset .  2)
     (cperl-extra-newline-before-brace .  t)
     (cperl-extra-newline-before-brace-multiline .  t)
     (cperl-merge-trailing-else	       .  nil))

    ("K&R"
     (cperl-indent-level               .  5)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -5)
     (cperl-label-offset               . -5)
     (cperl-continued-statement-offset .  5)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     ;;(cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else	       .  nil))

    ("BSD"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -4)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     ;;(cperl-extra-newline-before-brace-multiline .  nil)
     ;;(cperl-merge-trailing-else	       .  nil) ; ???
     )

    ("C++"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -4)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     (cperl-extra-newline-before-brace .  t)
     (cperl-extra-newline-before-brace-multiline .  t)
     (cperl-merge-trailing-else	       .  nil))

    ("Whitesmith"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     ;;(cperl-extra-newline-before-brace-multiline .  nil)
     ;;(cperl-merge-trailing-else	       .  nil) ; ???
     )
    ("Current"))
  "List of variables to set to get a particular indentation style.
Should be used via `cperl-set-style', command `cperl-file-style' or via
Perl menu.

See examples in `cperl-style-examples'.")

(defun cperl-set-style (style)
  "Set CPerl indentation variables to STYLE, one of the predefined styles.
This command sets the default values for the variables.  It does
not affect buffers visiting files where the style has been set as
a file or directory variable.  To change the indentation style of
a buffer, use the command `cperl-file-style' instead.
The arguments are a string representing the desired style.
The list of styles is in `cperl-style-alist', available styles
are \"CPerl\", \"PBP\", \"PerlStyle\", \"GNU\", \"K&R\", \"BSD\", \"C++\"
and \"Whitesmith\".

The current value of style is memorized (unless there is a memorized
data already), may be restored by `cperl-set-style-back'.

Choosing \"Current\" style will not change style, so this may be used for
side-effect of memorizing only.  Examples in `cperl-style-examples'."
  (interactive
   (list (completing-read "Enter style: " cperl-style-alist nil 'insist)))
  (or cperl-old-style
      (setq cperl-old-style
            (mapcar (lambda (name)
                      (cons name (eval name)))
		    cperl-styles-entries)))
  (let ((style (cdr (assoc style cperl-style-alist))) setting)
    (while style
      (setq setting (car style) style (cdr style))
      (set-default-toplevel-value (car setting) (cdr setting))))
  (set-default-toplevel-value 'cperl-file-style style))

(defun cperl-set-style-back ()
  "Restore a style memorized by `cperl-set-style'."
  (interactive)
  (or cperl-old-style (error "The style was not changed"))
  (let (setting)
    (while cperl-old-style
      (setq setting (car cperl-old-style)
	    cperl-old-style (cdr cperl-old-style))
      (set-default-toplevel-value (car setting) (cdr setting)))))

(defun cperl-file-style (style)
  "Set the indentation style for the current buffer to STYLE.
The list of styles is in `cperl-style-alist', available styles
are \"CPerl\", \"PBP\", \"PerlStyle\", \"GNU\", \"K&R\", \"BSD\", \"C++\"
and \"Whitesmith\"."
  (interactive
   (list (completing-read "Enter style: " cperl-style-alist nil 'insist)))
  (dolist (setting (cdr (assoc style cperl-style-alist)) style)
    (let ((option (car setting))
          (value (cdr setting)))
      (set (make-local-variable option) value)))
  (setq-local cperl-file-style style))

(declare-function Info-find-node "info"
		  (filename nodename &optional no-going-back strict-case
                            noerror))

(defun cperl-info-buffer (type)
  "Return buffer with documentation.  Create if missing.
If TYPE, this vars buffer.
Special care is taken to not stomp over an existing info buffer"
  (let* ((bname (if type "*info-perl-var*" "*info-perl*"))
	 (info (get-buffer bname))
	 (oldbuf (get-buffer "*info*")))
    (if info info
      (save-window-excursion
	;; Get Info running
	(require 'info)
	(cond (oldbuf
	       (set-buffer oldbuf)
	       (rename-buffer "*info-perl-tmp*")))
	(save-window-excursion
	  (info))
	(Info-find-node cperl-info-page (if type "perlvar" "perlfunc"))
	(set-buffer "*info*")
	(rename-buffer bname)
	(cond (oldbuf
	       (set-buffer "*info-perl-tmp*")
	       (rename-buffer "*info*")
	       (set-buffer bname)))
        (setq-local window-min-height 2)
	(current-buffer)))))

(defun cperl-word-at-point (&optional p)
  "Return the word at point or at P."
  (save-excursion
    (if p (goto-char p))
    (or (cperl-word-at-point-hard)
	(progn
	  (require 'etags)
	  (funcall (or (and (boundp 'find-tag-default-function)
			    find-tag-default-function)
		       (get major-mode 'find-tag-default-function)
		       'find-tag-default))))))

(defun cperl-info-on-command (command)
  "Show an info buffer for COMMAND.
This is obsolete because Perl info pages are no longer distributed."
  (declare (obsolete cperl-perldoc "30.1"))
  (interactive
   (let* ((default (cperl-word-at-point))
	  (read (read-string
		 (cperl--format-prompt "Find doc for Perl function" default))))
     (list (if (equal read "")
	       default
	     read))))

  (let ((cmd-desc (concat "^" (regexp-quote command) "[^a-zA-Z_0-9]")) ; "tr///"
	pos isvar height iniheight frheight buf win iniwin not-loner
	max-height char-height buf-list)
    (if (string-match "^-[a-zA-Z]$" command)
	(setq cmd-desc "^-X[ \t\n]"))
    (setq isvar (string-match "^[$@%]" command)
	  buf (cperl-info-buffer isvar)
	  iniwin (selected-window))
    (set-buffer buf)
    (goto-char (point-min))
    (or isvar
	(progn (re-search-forward "^-X[ \t\n]")
	       (forward-line -1)))
    (if (re-search-forward cmd-desc nil t)
	(progn
	  ;; Go back to beginning of the group (ex, for qq)
	  (if (re-search-backward "^[ \t\n\f]")
	      (forward-line 1))
	  (beginning-of-line)
	  ;; Get some of
	  (setq pos (point)
		buf-list (list buf "*info-perl-var*" "*info-perl*"))
	  (while (and (not win) buf-list)
	    (setq win (get-buffer-window (car buf-list) t))
	    (setq buf-list (cdr buf-list)))
	  (or (not win)
	      (eq (window-buffer win) buf)
	      (set-window-buffer win buf))
	  (pop-to-buffer buf)
	  (goto-char pos)		; Needed (?!).
	  ;; Resize
	  (setq iniheight (window-height)
		frheight (frame-height)
		not-loner (< iniheight (1- frheight))) ; Are not alone
	  (cond ((if not-loner cperl-max-help-size
		   cperl-shrink-wrap-info-frame)
		 (setq height
		       (+ 2
			  (count-lines
			   pos
			   (save-excursion
			     (if (re-search-forward
				  "^[ \t][^\n]*\n+\\([^ \t\n\f]\\|\\'\\)" nil t)
				 (match-beginning 0) (point-max)))))
		       max-height
		       (if not-loner
			   (/ (* (- frheight 3) cperl-max-help-size) 100)
			 (setq char-height (frame-char-height))
			 (if (eq char-height 1) (setq char-height 18))
			 ;; Title, menubar, + 2 for slack
			 (- (/ (display-pixel-height) char-height) 4)))
		 (if (> height max-height) (setq height max-height))
		 ;;(message "was %s doing %s" iniheight height)
		 (if not-loner
		     (enlarge-window (- height iniheight))
		   (set-frame-height (window-frame win) (1+ height)))))
	  (set-window-start (selected-window) pos))
      (message "No entry for %s found." command))
    ;;(pop-to-buffer buffer)
    (select-window iniwin)))

(defun cperl-info-on-current-command ()
  "Show an info buffer for the current command.
This is obsolete because Perl info pages are no longer distributed."
  (declare (obsolete cperl-perldoc "30.1"))
  (interactive)
  (cperl-perldoc (cperl-word-at-point)))

(defun cperl-imenu-info-imenu-search ()
  "Search a Perl info buffer.
This is obsolete because Perl info pages are no longer distributed."
  (declare (obsolete nil "30.1"))
  (if (looking-at "^-X[ \t\n]") nil
    (re-search-backward
     "^\n\\([-a-zA-Z_]+\\)[ \t\n]")
    (forward-line 1)))

(defun cperl-imenu-info-imenu-name ()
  "Return the name of a Perl info buffer.
This is obsolete because Perl info pages are no longer distributed."
  (declare (obsolete nil "30.1"))
  (buffer-substring
   (match-beginning 1) (match-end 1)))

(declare-function imenu-choose-buffer-index "imenu" (&optional prompt alist))

(defun cperl-imenu-on-info ()
  "Create an imenu index for a Perl info page.
This is obsolete because Perl info pages are no longer distributed."
  (declare (obsolete nil "30.1"))
  (interactive)
  (message
   (concat "The info file `perl' is no longer available.\n"
           "Consider installing the perl-doc package from GNU ELPA.")))

(defun cperl-lineup (beg end &optional step minshift)
  "Lineup construction in a region from BEG to END.
Beginning of region should be at the start of a construction.
All first occurrences of this construction in the lines that are
partially contained in the region are lined up at the same column.

MINSHIFT is the minimal amount of space to insert before the construction.
STEP is the tabwidth to position constructions.
If STEP is nil, `cperl-lineup-step' will be used
\(or `cperl-indent-level', if `cperl-lineup-step' is nil).
Will not move the position at the start to the left."
  (interactive "r")
  (let (search col tcol seen)
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end (point-marker))
      (goto-char beg)
      (skip-chars-forward " \t\f")
      (setq beg (point-marker))
      (indent-region beg end nil)
      (goto-char beg)
      (setq col (current-column))
      ;; Assuming that lineup is done on Perl syntax, this regexp
      ;; doesn't need to be unicode aware -- haj, 2021-09-10
      (if (looking-at "[a-zA-Z0-9_]")
	  (if (looking-at "\\<[a-zA-Z0-9_]+\\>")
	      (setq search
		    (concat "\\<"
			    (regexp-quote
			     (buffer-substring (match-beginning 0)
					       (match-end 0))) "\\>"))
	    (error "Cannot line up in a middle of the word"))
	(if (looking-at "$")
	    (error "Cannot line up end of line"))
	(setq search (regexp-quote (char-to-string (following-char)))))
      (setq step (or step cperl-lineup-step cperl-indent-level))
      (or minshift (setq minshift 1))
      (while (progn
	       (beginning-of-line 2)
	       (and (< (point) end)
		    (re-search-forward search end t)
		    (goto-char (match-beginning 0))))
	(setq tcol (current-column) seen t)
	(if (> tcol col) (setq col tcol)))
      (or seen
	  (error "The construction to line up occurred only once"))
      (goto-char beg)
      (setq col (+ col minshift))
      (if (/= (% col step) 0) (setq step (* step (1+ (/ col step)))))
      (while
	  (progn
	    (cperl-make-indent col)
	    (beginning-of-line 2)
	    (and (< (point) end)
		 (re-search-forward search end t)
		 (goto-char (match-beginning 0)))))))) ; No body

(defun cperl-etags (&optional add all files) ;; NOT USED???
  "Run etags with appropriate options for Perl FILES.
Add to the current tags file if ADD is non-nil.
If optional argument ALL is `recursive', will process Perl files
in subdirectories too."
  ;; Apparently etags doesn't support UTF-8 encoded sources, and usage
  ;; of etags has been commented out in the menu since ... well,
  ;; forever.  So, let's just stick to ASCII here. -- haj, 2021-09-14
  (interactive)
  (let ((cmd etags-program-name)
	(args `("-l" "none" "-r"
		;;                        1=fullname  2=package?             3=name                       4=proto?             5=attrs? (VERY APPROX!)
		,(concat
		  "/\\<" cperl-sub-regexp "[ \\t]+\\(\\([a-zA-Z0-9:_]*::\\)?\\([a-zA-Z0-9_]+\\)\\)[ \\t]*\\(([^()]*)[ \t]*\\)?\\([ \t]*:[^#{;]*\\)?\\([{#]\\|$\\)/\\3/")
		"-r"
		"/\\<package[ \\t]+\\(\\([a-zA-Z0-9:_]*::\\)?\\([a-zA-Z0-9_]+\\)\\)[ \\t]*\\([#;]\\|$\\)/\\1/"
		"-r"
		"/\\<\\(package\\)[ \\t]*;/\\1;/"))
	res)
    (if add (setq args (cons "-a" args)))
    (or files (setq files (list buffer-file-name)))
    (cond
     ((eq all 'recursive)
      ;;(error "Not implemented: recursive")
      (setq args (append (list "-e"
			       "sub wanted {push @ARGV, $File::Find::name if /\\.[pP][Llm]$/}
				use File::Find;
				find(\\&wanted, '.');
				exec @ARGV;"
			       cmd) args)
	    cmd "perl"))
     (all
      ;;(error "Not implemented: all")
      (setq args (append (list "-e"
			       "push @ARGV, <*.PL *.pl *.pm>;
				exec @ARGV;"
			       cmd) args)
	    cmd "perl"))
     (t
      (setq args (append args files))))
    (setq res (apply 'call-process cmd nil nil nil args))
    (or (eq res 0)
	(message "etags returned \"%s\"" res))))

(defun cperl-toggle-auto-newline ()
  "Toggle the state of `cperl-auto-newline'."
  (interactive)
  (setq cperl-auto-newline (not cperl-auto-newline))
  (message "Newlines will %sbe auto-inserted now."
	   (if cperl-auto-newline "" "not ")))

(defun cperl-toggle-abbrev ()
  "Toggle the state of automatic keyword expansion in CPerl mode."
  (interactive)
  (abbrev-mode (if abbrev-mode 0 1))
  (message "Perl control structure will %sbe auto-inserted now."
	   (if abbrev-mode "" "not ")))


(defun cperl-toggle-electric ()
  "Toggle the state of parentheses doubling in CPerl mode."
  (interactive)
  (setq cperl-electric-parens (if (cperl-val 'cperl-electric-parens) 'null t))
  (message "Parentheses will %sbe auto-doubled now."
	   (if (cperl-val 'cperl-electric-parens) "" "not ")))

(defun cperl-toggle-autohelp ()
  ;; FIXME: Turn me into a minor mode.  Fix menu entries for "Auto-help on" as
  ;; well.
  "Toggle the state of Auto-Help on Perl constructs (put in the message area).
Delay of auto-help controlled by `cperl-lazy-help-time'."
  (interactive)
  (if cperl-lazy-installed
      (cperl-lazy-unstall)
    (cperl-lazy-install))
  (message "Perl help messages will %sbe automatically shown now."
	   (if cperl-lazy-installed "" "not ")))

(defun cperl-toggle-construct-fix ()
  "Toggle whether `indent-region'/`indent-sexp' fix whitespace too."
  (interactive)
  (setq cperl-indent-region-fix-constructs
	(if cperl-indent-region-fix-constructs
	    nil
	  1))
  (message "indent-region/indent-sexp will %sbe automatically fix whitespace."
	   (if cperl-indent-region-fix-constructs "" "not ")))

(defun cperl-toggle-set-debug-unwind (arg &optional backtrace)
  "Toggle (or, with numeric argument, set) debugging state of syntaxification.
Nonpositive numeric argument ARG disables debugging messages.  The
message summarizes which regions it was decided to rescan for syntactic
constructs.  BACKTRACE is added to ARG if provided.

The message looks like this:

  Syxify req=123..138 actual=101..146 done-to: 112=>146 statepos: 73=>117

Numbers are character positions in the buffer.  REQ provides the range to
rescan requested by `font-lock'.  ACTUAL is the range actually resyntaxified;
for correct operation it should start and end outside any special syntactic
construct.  DONE-TO and STATEPOS indicate changes to internal caches maintained
by CPerl."
  (interactive "P")
  (or arg
      (setq arg (if (eq cperl-syntaxify-by-font-lock
			(if backtrace 'backtrace 'message))
                    0 1)))
  (setq arg (if (> arg 0) (if backtrace 'backtrace 'message) t))
  (setq cperl-syntaxify-by-font-lock arg)
  (message "Debugging messages of syntax unwind %sabled."
	   (if (eq arg t) "dis" "en")))

;;;; Tags file creation.

(defvar cperl-tmp-buffer " *cperl-tmp*")

(defun cperl-setup-tmp-buf ()
  "Prepare a temporary buffer for internal use during tags file creation."
  (set-buffer (get-buffer-create cperl-tmp-buffer))
  (set-syntax-table cperl-mode-syntax-table)
  (buffer-disable-undo)
  (auto-fill-mode 0)
  (if cperl-use-syntax-table-text-property-for-tags
      (progn
	;; Do not introduce variable if not needed, we check it!
        (setq-local parse-sexp-lookup-properties t))))

;; Copied from imenu-example--name-and-position.
(defvar imenu-use-markers)

(defun cperl-imenu-name-and-position ()
  "Return the current/previous sexp and its (beginning) location.
Does not move point."
  (save-excursion
    (forward-sexp -1)
    (let ((beg (if imenu-use-markers (point-marker) (point)))
	  (end (progn (forward-sexp) (point))))
      (cons (buffer-substring beg end)
	    beg))))

(defun cperl-xsub-scan ()
  "Scan for XS subroutines."
  (require 'imenu)
  (let ((index-alist '())
        index index1 name package prefix)
    (goto-char (point-min))
    ;; Search for the function
    (progn ;;save-match-data
      (while (re-search-forward
              ;; FIXME: Should XS code be unicode aware?  Recent C
              ;; compilers (Gcc 10+) are, but I guess this isn't used
              ;; much. -- haj, 2021-09-14
	      "^\\([ \t]*MODULE\\>[^\n]*\\<PACKAGE[ \t]*=[ \t]*\\([a-zA-Z_][a-zA-Z_0-9:]*\\)\\>\\|\\([a-zA-Z_][a-zA-Z_0-9]*\\)(\\|[ \t]*BOOT:\\)"
	      nil t)
	(cond
	 ((match-beginning 2)		; SECTION
	  (setq package (buffer-substring (match-beginning 2) (match-end 2)))
	  (goto-char (match-beginning 0))
	  (skip-chars-forward " \t")
	  (forward-char 1)
	  (if (looking-at "[^\n]*\\<PREFIX[ \t]*=[ \t]*\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\>")
	      (setq prefix (buffer-substring (match-beginning 1) (match-end 1)))
	    (setq prefix nil)))
	 ((not package) nil)		; C language section
	 ((match-beginning 3)		; XSUB
	  (goto-char (1+ (match-beginning 3)))
	  (setq index (cperl-imenu-name-and-position))
	  (setq name (buffer-substring (match-beginning 3) (match-end 3)))
	  (if (and prefix (string-match (concat "^" prefix) name))
	      (setq name (substring name (length prefix))))
	  (cond ((string-match "::" name) nil)
		(t
		 (setq index1 (cons (concat package "::" name) (cdr index)))
		 (push index1 index-alist)))
	  (setcar index name)
	  (push index index-alist))
	 (t				; BOOT: section
	  ;; (beginning-of-line)
	  (setq index (cperl-imenu-name-and-position))
	  (setcar index (concat package "::BOOT:"))
	  (push index index-alist)))))
    index-alist))

(defvar cperl-unreadable-ok nil)

(defun cperl-find-tags (ifile xs topdir)
  "Find tags in IFILE.  Treat as an XS file if non-nil.
Entries are made relative to TOPDIR."
  (let ((b (get-buffer cperl-tmp-buffer)) ind lst elt pos ret rel
	(cperl-pod-here-fontify nil) file)
    (save-excursion
      (if b (set-buffer b)
	(cperl-setup-tmp-buf))
      (erase-buffer)
      (condition-case nil
	  (setq file (car (insert-file-contents ifile)))
	(error (if cperl-unreadable-ok nil
		 (if (y-or-n-p
		      (format "File %s unreadable.  Continue? " ifile))
		     (setq cperl-unreadable-ok t)
		   (error "Aborting: unreadable file %s" ifile)))))
      (if (not file)
	  (message "Unreadable file %s" ifile)
	(message "Scanning file %s ..." file)
	(if (and cperl-use-syntax-table-text-property-for-tags
		 (not xs))
	    (condition-case err		; after __END__ may have garbage
		(cperl-find-pods-heres nil nil noninteractive)
	      (error (message "While scanning for syntax: %S" err))))
	(if xs
	    (setq lst (cperl-xsub-scan))
	  (setq ind (cperl-imenu--create-perl-index))
	  (setq lst (cdr (assoc "+Unsorted List+..." ind))))
	(setq lst
	      (mapcar
               (lambda (elt)
                 (cond ((string-match (rx line-start (or alpha "_")) (car elt))
                        (goto-char (cdr elt))
                        (beginning-of-line) ; pos should be of the start of the line
                        (list (car elt)
                              (point)
                              (1+ (count-lines 1 (point))) ; 1+ since at beg-o-l
                              (buffer-substring (progn
                                                  (goto-char (cdr elt))
                                                  ;; After name now...
                                                  (or (eolp) (forward-char 1))
                                                  (point))
                                                (progn
                                                  (beginning-of-line)
                                                  (point)))))))
	       lst))
	(erase-buffer)
	(while lst
	  (setq elt (car lst) lst (cdr lst))
	  (if elt
	      (progn
		(insert (elt elt 3)
			127
			(if (string-match "^package " (car elt))
			    (substring (car elt) 8)
			  (if (string-match "^class " (car elt))
			      (substring (car elt) 6)
			    (car elt)))
			1
			(number-to-string (elt elt 2)) ; Line
			","
			(number-to-string (1- (elt elt 1))) ; Char pos 0-based
			"\n")
		(if (and (string-match (rx line-start
                                           (eval cperl--basic-identifier-rx) "++")
                                       (car elt))
                         (string-match (rx-to-string `(sequence line-start
                                                                (regexp ,cperl-sub-regexp)
                                                                (1+ (in " \t"))
                                                                ,cperl--normal-identifier-rx))
                                       (elt elt 3)))
		    ;; Need to insert the name without package as well
		    (setq lst (cons (cons (substring (elt elt 3)
						     (match-beginning 1)
						     (match-end 1))
					  (cdr elt))
				    lst))))))
	(setq pos (point))
	(goto-char 1)
	(setq rel file)
	;; On case-preserving filesystems case might be encoded in properties
	(set-text-properties 0 (length rel) nil rel)
	(and (equal topdir (substring rel 0 (length topdir)))
	     (setq rel (substring file (length topdir))))
	(insert "\f\n" rel "," (number-to-string (1- pos)) "\n")
	(setq ret (buffer-substring 1 (point-max)))
	(erase-buffer)
	(or noninteractive
	    (message "Scanning file %s finished" file))
	ret))))

(defun cperl-add-tags-recurse-noxs ()
  "Add to TAGS data for \"pure\" Perl files in the current directory and kids.
Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \\
        -f cperl-add-tags-recurse-noxs"
  (cperl-write-tags nil nil t t nil t))

(defun cperl-add-tags-recurse-noxs-fullpath ()
  "Add to TAGS data for \"pure\" Perl in the current directory and kids.
Writes down fullpath, so TAGS is relocatable (but if the build directory
is relocated, the file TAGS inside it breaks). Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \\
        -f cperl-add-tags-recurse-noxs-fullpath"
  (cperl-write-tags nil nil t t nil t ""))

(defun cperl-add-tags-recurse ()
  "Add to TAGS file data for Perl files in the current directory and kids.
Use as
  Emacs -batch -q -no-site-file -l emacs/cperl-mode.el \\
        -f `cperl-add-tags-recurse'"
  (cperl-write-tags nil nil t t))

(defvar cperl-tags-file-name "TAGS"
  "TAGS file name to use in `cperl-write-tags'.")

(declare-function initialize-new-tags-table "etags" ())

(defun cperl-write-tags (&optional file erase recurse dir inbuffer noxs topdir)
  "Write tags for FILE.  If this is a directory, RECURSE if non-nil.
If ERASE is `ignore', do not erase, and do not try to delete old info.
If INBUFFER, do not select buffer, and do not save."
  (require 'etags)
  (if file nil
    (setq file (if dir default-directory (buffer-file-name)))
    (if (and (not dir) (buffer-modified-p)) (error "Save buffer first!")))
  (or topdir
      (setq topdir default-directory))
  (let ((tags-file-name cperl-tags-file-name)
        (inhibit-read-only t)
	(case-fold-search nil)
	xs rel)
    (save-excursion
      (cond (inbuffer nil)		; Already there
	    ((file-exists-p tags-file-name)
	     (visit-tags-table-buffer tags-file-name))
	    (t
             (set-buffer (find-file-noselect tags-file-name))))
      (cond
       (dir
	(cond ((eq erase 'ignore))
	      (erase
	       (erase-buffer)
	       (setq erase 'ignore)))
	(let ((files
	       (condition-case nil
		   (directory-files file t
				    (if recurse nil cperl-scan-files-regexp)
				    t)
		 (error
		  (if cperl-unreadable-ok nil
		    (if (y-or-n-p
			 (format "Directory %s unreadable.  Continue? " file))
			(progn
                          (setq cperl-unreadable-ok t)
                          nil)	; Return empty list
		      (error "Aborting: unreadable directory %s" file)))))))
          (mapc (lambda (file)
                  (cond
                   ((string-match cperl-noscan-files-regexp file)
                    nil)
                   ((not (file-directory-p file))
                    (if (string-match cperl-scan-files-regexp file)
                        (cperl-write-tags file erase recurse nil t noxs topdir)))
                   ((not recurse) nil)
                   (t (cperl-write-tags file erase recurse t t noxs topdir))))
		files)))
       (t
	(setq xs (string-match "\\.xs$" file))
	(if (not (and xs noxs))
	    (progn
	      (cond ((eq erase 'ignore) (goto-char (point-max)))
		    (erase (erase-buffer))
		    (t
		     (goto-char 1)
		     (setq rel file)
		     ;; On case-preserving filesystems case might be encoded in properties
		     (set-text-properties 0 (length rel) nil rel)
		     (and (equal topdir (substring rel 0 (length topdir)))
			  (setq rel (substring file (length topdir))))
		     (if (search-forward (concat "\f\n" rel ",") nil t)
			 (progn
			   (search-backward "\f\n")
			   (delete-region (point)
					  (save-excursion
					    (forward-char 1)
					    (if (search-forward "\f\n"
								nil 'toend)
						(- (point) 2)
					      (point-max)))))
		       (goto-char (point-max)))))
	      (insert (cperl-find-tags file xs topdir))))))
      (if inbuffer nil			; Delegate to the caller
	(save-buffer 0)			; No backup
        (initialize-new-tags-table)))))

(defvar cperl-tags-hier-regexp-list
  (concat
   "^\\("
      "\\(package\\)\\>"
     "\\|"
      cperl-sub-regexp "\\>[^\n]+::"
     "\\|"
      "[a-zA-Z_][a-zA-Z_0-9:]*(\C-?[^\n]+::" ; XSUB?
     "\\|"
      "[ \t]*BOOT:\C-?[^\n]+::"		; BOOT section
   "\\)"))

(defvar cperl-hierarchy '(() ())
  "Global hierarchy of classes.")

;; Follows call to (autoloaded) visit-tags-table.
(declare-function file-of-tag "etags" (&optional relative))
(declare-function etags-snarf-tag "etags" (&optional use-explicit))

(defun cperl-tags-hier-fill ()
  "Fill a tags hierarchy if in a tag table cooked by cperl."
  (goto-char 1)
  (let (pack name line ord cons1 file info fileind)
    (while (re-search-forward cperl-tags-hier-regexp-list nil t)
      (setq pack (match-beginning 2))
      (beginning-of-line)
      (if (looking-at (concat
		       "\\([^\n]+\\)"
		       "\C-?"
		       "\\([^\n]+\\)"
		       "\C-a"
		       "\\([0-9]+\\)"
		       ","
		       "\\([0-9]+\\)"))
	  (progn
	    (setq ;;str (buffer-substring (match-beginning 1) (match-end 1))
		  name (buffer-substring (match-beginning 2) (match-end 2))
		  ;;pos (buffer-substring (match-beginning 3) (match-end 3))
		  line (buffer-substring (match-beginning 3) (match-end 3))
		  ord (if pack 1 0)
		  file (file-of-tag)
		  fileind (format "%s:%s" file line)
		  ;; Moves to beginning of the next line:
		  info (etags-snarf-tag))
	    ;; Move back
	    (forward-char -1)
	    ;; Make new member of hierarchy name ==> file ==> pos if needed
	    (if (setq cons1 (assoc name (nth ord cperl-hierarchy)))
		;; Name known
		(setcdr cons1 (cons (cons fileind (vector file info))
				    (cdr cons1)))
	      ;; First occurrence of the name, start alist
	      (setq cons1 (cons name (list (cons fileind (vector file info)))))
	      (if pack
		  (setcar (cdr cperl-hierarchy)
			  (cons cons1 (nth 1 cperl-hierarchy)))
		(setcar cperl-hierarchy
			(cons cons1 (car cperl-hierarchy)))))))
      (end-of-line))))

(declare-function x-popup-menu "menu.c" (position menu))
(declare-function etags-goto-tag-location "etags" (tag-info))

(defun cperl-tags-hier-init (&optional update)
  "Show hierarchical menu of classes and methods.
Finds info about classes by a scan of loaded TAGS files.
Supposes that the TAGS files contain fully qualified function names.
One may build such TAGS files from CPerl mode menu.
If UPDATE is non-nil, update the tags table."
  (interactive)
  (require 'etags)
  (require 'imenu)
  (if (or update (null (nth 2 cperl-hierarchy)))
      (let ((remover (lambda (elt) ; (name (file1...) (file2..))
                       (or (nthcdr 2 elt)
                           ;; Only in one file
                           (setcdr elt (cdr (nth 1 elt))))))
	    to) ;; l1 l2 l3
	;; (setq cperl-hierarchy '(() () ())) ; Would write into '() later!
	(setq cperl-hierarchy (list () () ())) ;; (list l1 l2 l3)
	(or tags-table-list
	    (call-interactively 'visit-tags-table))
	(mapc
         (lambda (tagsfile)
           (message "Updating list of classes... %s" tagsfile)
           (set-buffer (get-file-buffer tagsfile))
           (cperl-tags-hier-fill))
	 tags-table-list)
	(message "Updating list of classes... postprocessing...")
	(mapc remover (car cperl-hierarchy))
	(mapc remover (nth 1 cperl-hierarchy))
	(setq to (list nil (cons "Packages: " (nth 1 cperl-hierarchy))
		       (cons "Methods: " (car cperl-hierarchy))))
	(cperl-tags-treeify to 1)
	(setcar (nthcdr 2 cperl-hierarchy)
		(cperl-menu-to-keymap (cons '("+++UPDATE+++" . -999) (cdr to))))
	(message "Updating list of classes: done, requesting display...")))
  (or (nth 2 cperl-hierarchy)
      (error "No items found"))
  (setq update
        ;; (imenu-choose-buffer-index "Packages: " (nth 2 cperl-hierarchy))
        (if (display-popup-menus-p)
	    (x-popup-menu t (nth 2 cperl-hierarchy))
	  (require 'tmm)
	  (tmm-prompt (nth 2 cperl-hierarchy))))
  (if (and update (listp update))
      (progn (while (cdr update) (setq update (cdr update)))
	     (setq update (car update)))) ; Get the last from the list
  (if (vectorp update)
      (progn
	(find-file (elt update 0))
	(etags-goto-tag-location (elt update 1))))
  (if (eq update -999) (cperl-tags-hier-init t)))

(defun cperl-tags-treeify (to level)
  "Build a tree for the tags hierarchy into TO.
LEVEL us the current level during the recursive calls."
  ;; cadr of `to' is read-write.  On start it is a cons
  (let* ((regexp (concat "^\\(" (mapconcat
				 #'identity
				 (make-list level "[_a-zA-Z0-9]+")
				 "::")
			 "\\)\\(::\\)?"))
	 (packages (cdr (nth 1 to)))
	 (methods (cdr (nth 2 to)))
	 head cons1 cons2 ord writeto recurse ;; l1
	 root-packages root-functions
	 (move-deeper
          (lambda (elt)
            (cond ((and (string-match regexp (car elt))
                        (or (eq ord 1) (match-end 2)))
                   (setq head (substring (car elt) 0 (match-end 1))
                         recurse t)
                   (if (setq cons1 (assoc head writeto)) nil
                     ;; Need to init new head
                     (setcdr writeto (cons (list head (list "Packages: ")
                                                 (list "Methods: "))
                                           (cdr writeto)))
                     (setq cons1 (nth 1 writeto)))
                   (setq cons2 (nth ord cons1)) ; Either packs or meths
                   (setcdr cons2 (cons elt (cdr cons2))))
                  ((eq ord 2)
                   (setq root-functions (cons elt root-functions)))
                  (t
                   (setq root-packages (cons elt root-packages)))))))
    (setcdr to nil) ;; l1		; Init to dynamic space
    (setq writeto to)
    (setq ord 1)
    (mapc move-deeper packages)
    (setq ord 2)
    (mapc move-deeper methods)
    (if recurse
        (mapc (lambda (elt)
                (cperl-tags-treeify elt (1+ level)))
	      (cdr to)))
    ;;Now clean up leaders with one child only
    (mapc (lambda (elt)
            (if (not (and (listp (cdr elt))
                          (eq (length elt) 2)))
                nil
              (setcar elt (car (nth 1 elt)))
              (setcdr elt (cdr (nth 1 elt)))))
	  (cdr to))
    ;; Sort the roots of subtrees
    (if (default-value 'imenu-sort-function)
	(setcdr to
		(sort (cdr to) (default-value 'imenu-sort-function))))
    ;; Now add back functions removed from display
    (mapc (lambda (elt)
            (setcdr to (cons elt (cdr to))))
	  (if (default-value 'imenu-sort-function)
	      (nreverse
	       (sort root-functions (default-value 'imenu-sort-function)))
	    root-functions))
    ;; Now add back packages removed from display
    (mapc (lambda (elt)
            (setcdr to (cons (cons (concat "package " (car elt))
                                   (cdr elt))
                             (cdr to))))
	  (if (default-value 'imenu-sort-function)
	      (nreverse
	       (sort root-packages (default-value 'imenu-sort-function)))
	    root-packages))))

(defun cperl-list-fold (list name limit)
  "Fold LIST with name NAME into sublists with LIMIT members or less."
  (let (list1 list2 elt1 (num 0))
    (if (<= (length list) limit) list
      (setq list1 nil list2 nil)
      (while list
	(setq num (1+ num)
	      elt1 (car list)
	      list (cdr list))
	(if (<= num imenu-max-items)
	    (setq list2 (cons elt1 list2))
	  (setq list1 (cons (cons name
				  (nreverse list2))
			    list1)
		list2 (list elt1)
		num 1)))
      (nreverse (cons (cons name
			    (nreverse list2))
		      list1)))))

(defun cperl-menu-to-keymap (menu)
  "Prepare MENU for display with `imenu'."
  (let (list)
    (cons 'keymap
	  (mapcar
           (lambda (elt)
             (cond ((listp (cdr elt))
                    (setq list (cperl-list-fold
                                (cdr elt) (car elt) imenu-max-items))
                    (cons nil
                          (cons (car elt)
                                (cperl-menu-to-keymap list))))
                   (t
                    (list (cdr elt) (car elt) t)))) ; t is needed in 19.34
	   (cperl-list-fold menu "Root" imenu-max-items)))))


(defvar cperl-bad-style-regexp
  (mapconcat #'identity
	     '("[^-\n\t <>=+!.&|(*/'`\"#^][-=+<>!|&^]" ; char sign
	       "[-<>=+^&|]+[^- \t\n=+<>~]") ; sign+ char
	     "\\|")
  "Finds places such that insertion of a whitespace may help a lot.")

(defvar cperl-not-bad-style-regexp
  (mapconcat
   #'identity
   '("[^-\t <>=+]\\(--\\|\\+\\+\\)"	; var-- var++
     "[a-zA-Z0-9_][|&][a-zA-Z0-9_$]"	; abc|def abc&def are often used.
     "&[(a-zA-Z0-9_$]"			; &subroutine &(var->field)
     "<\\$?\\sw+\\(\\.\\(\\sw\\|_\\)+\\)?>"	; <IN> <stdin.h>
     "-[a-zA-Z][ \t]+[_$\"'`a-zA-Z]"	; -f file, -t STDIN
     "-[0-9]"				; -5
     "\\+\\+"				; ++var
     "--"				; --var
     ".->"				; a->b
     "->"				; a SPACE ->b
     "\\[-"				; a[-1]
     "\\\\[&$@*\\]"			; \&func
     "^="				; =head
     "\\$."				; $|
     "<<[a-zA-Z_'\"`]"			; <<FOO, <<'FOO'
     "||"
     "//"
     "&&"
     "[CBIXSLFZ]<\\(\\sw\\|\\s \\|\\s_\\|[\n]\\)*>" ; C<code like text>
     "-[a-zA-Z_0-9]+[ \t]*=>"		; -option => value
     ;; Unaddressed trouble spots: = -abc, f(56, -abc) --- specialcased below
     ;;"[*/+-|&<.]+="
     )
   "\\|")
  "Lisp Regular expression for things regularly occurring in a Perl regex.
If matches at the start of match found by `my-bad-c-style-regexp',
insertion of a whitespace will not help.")

(defvar found-bad)

(defun cperl-find-bad-style ()
  "Find places in the buffer where insertion of a whitespace may help.
Prompts user for insertion of spaces.
Currently it is tuned to C and Perl syntax."
  (interactive)
  (let (found-bad (p (point)))
    (setq last-nonmenu-event 13)	; To disable popup
    (goto-char (point-min))
    (map-y-or-n-p "Insert space here? "
		  (lambda (_) (insert " "))
		  'cperl-next-bad-style
		  '("location" "locations" "insert a space into")
		  `((?\C-r ,(lambda (_)
			      (let ((buffer-quit-function
				     #'exit-recursive-edit))
			        (message "Exit with Esc Esc")
			        (recursive-edit)
			        t))	; Consider acted upon
			   "edit, exit with Esc Esc")
		    (?e ,(lambda (_)
			   (let ((buffer-quit-function
				  #'exit-recursive-edit))
			     (message "Exit with Esc Esc")
			     (recursive-edit)
			     t))        ; Consider acted upon
			"edit, exit with Esc Esc"))
		  t)
    (if found-bad (goto-char found-bad)
      (goto-char p)
      (message "No appropriate place found"))))

(defun cperl-next-bad-style ()
  "Proceed to the next occurrence of bad regexp style."
  (let (p (not-found t) found)
    (while (and not-found
		(re-search-forward cperl-bad-style-regexp nil 'to-end))
      (setq p (point))
      (goto-char (match-beginning 0))
      (if (or
	   (looking-at cperl-not-bad-style-regexp)
	   ;; Check for a < -b and friends
	   (and (eq (following-char) ?\-)
		(save-excursion
		  (skip-chars-backward " \t\n")
		  (memq (preceding-char) '(?\= ?\> ?\< ?\, ?\( ?\[ ?\{))))
	   ;; Now check for syntax type
	   (save-match-data
	     (setq found (point))
	     (beginning-of-defun)
	     (let ((pps (parse-partial-sexp (point) found)))
	       (or (nth 3 pps) (nth 4 pps) (nth 5 pps)))))
	  (goto-char (match-end 0))
	(goto-char (1- p))
	(setq not-found nil
	      found-bad found)))
    (not not-found)))


;;; Getting help
(defvar cperl-have-help-regexp
  ;;(concat "\\("
  (mapconcat
   #'identity
   '("[$@%*&][[:alnum:]_:]+\\([ \t]*[[{]\\)?" ; Usual variable
     "[$@]\\^[a-zA-Z]"			; Special variable
     "[$@][^ \n\t]"			; Special variable
     "-[a-zA-Z]"			; File test
     "\\\\[a-zA-Z0]"			; Special chars
     "^=[a-z][a-zA-Z0-9_]*"		; POD sections
     "[-!&*+,./<=>?\\^|~]+"		; Operator
     "[[:alnum:]_:]+"			; symbol or number
     "x="
     "#!")
   ;;"\\)\\|\\("
   "\\|")
  ;;"\\)"
  ;;)
  "Matches places in the buffer we can find help for.")

(defvar cperl-message-on-help-error t)
(defvar cperl-help-from-timer nil)

(defun cperl-word-at-point-hard ()
  "Try hard to find a useful token for Perl at point."
  ;; Does not save-excursion
  ;; Get to the something meaningful
  (or (eobp) (eolp) (forward-char 1))
  (re-search-backward "[-[:alnum:]_:!&*+,./<=>?\\^|~$%@]"
                      (line-beginning-position)
		      'to-beg)
  ;;  (cond
  ;;   ((or (eobp) (looking-at "[][ \t\n{}();,]")) ; Not at a symbol
  ;;    (skip-chars-backward " \n\t\r({[]});,")
  ;;    (or (bobp) (backward-char 1))))
  ;; Try to backtrace
  (cond
   ((looking-at "[[:alnum:]_:]")	; symbol
    (skip-chars-backward "[:alnum:]_:")
    (cond
     ((and (eq (preceding-char) ?^)	; $^I
	   (eq (char-after (- (point) 2)) ?\$))
      (forward-char -2))
     ((memq (preceding-char) (append "*$@%&\\" nil)) ; *glob
      (forward-char -1))
     ((and (eq (preceding-char) ?\=)
	   (eq (current-column) 1))
      (forward-char -1)))		; =head1
    (if (and (eq (preceding-char) ?\<)
             (looking-at "\\$?[[:alnum:]_:]+>")) ; <FH>
	(forward-char -1)))
   ((and (looking-at "=") (eq (preceding-char) ?x)) ; x=
    (forward-char -1))
   ((and (looking-at "\\^") (eq (preceding-char) ?\$)) ; $^I
    (forward-char -1))
   ((looking-at "[-!&*+,./<=>?\\^|~]")
    (skip-chars-backward "-!&*+,./<=>?\\^|~")
    (cond
     ((and (eq (preceding-char) ?\$)
	   (not (eq (char-after (- (point) 2)) ?\$))) ; $-
      (forward-char -1))
     ((and (eq (following-char) ?\>)
	   (string-match "[[:alnum:]_]" (char-to-string (preceding-char)))
	   (save-excursion
	     (forward-sexp -1)
	     (and (eq (preceding-char) ?\<)
		  (looking-at "\\$?[[:alnum:]_:]+>")))) ; <FH>
      (search-backward "<"))))
   ((and (eq (following-char) ?\$)
	 (eq (preceding-char) ?\<)
	 (looking-at "\\$?[[:alnum:]_:]+>")) ; <$fh>
    (forward-char -1)))
  (if (looking-at cperl-have-help-regexp)
      (buffer-substring (match-beginning 0) (match-end 0))))

(defun cperl-get-help ()
  "Get one-line docs on the symbol at the point.
The data for these docs is a little bit obsolete and may be in fact longer
than a line.  Your contribution to update/shorten it is appreciated."
  (interactive)
  (save-match-data			; May be called "inside" query-replace
    (save-excursion
      (let ((word (cperl-word-at-point-hard)))
	(if word
	    (if (and cperl-help-from-timer ; Bail out if not in mainland
		     (not (string-match "^#!\\|\\\\\\|^=" word)) ; Show help even in comments/strings.
		     (or (memq (get-text-property (point) 'face)
			       '(font-lock-comment-face font-lock-string-face))
			 (memq (get-text-property (point) 'syntax-type)
			       '(pod here-doc format))))
		nil
	      (cperl-describe-perl-symbol word))
	  (if cperl-message-on-help-error
	      (message "Nothing found for %s..."
		       (buffer-substring (point) (min (+ 5 (point)) (point-max))))))))))

;;; Stolen from perl-descr.el by Johan Vromans:

(defvar cperl-doc-buffer " *perl-doc*"
  "Where the documentation can be found.")

(defun cperl-describe-perl-symbol (val)
  "Display the documentation of symbol VAL at point, a Perl operator."
  (let ((enable-recursive-minibuffers t)
	regexp)
    (cond
     ((string-match "^[&*][a-zA-Z_]" val)
      (setq val (concat (substring val 0 1) "NAME")))
     ((string-match "^[$@]\\([a-zA-Z_:0-9]+\\)[ \t]*\\[" val)
      (setq val (concat "@" (substring val 1 (match-end 1)))))
     ((string-match "^[$@]\\([a-zA-Z_:0-9]+\\)[ \t]*{" val)
      (setq val (concat "%" (substring val 1 (match-end 1)))))
     ((and (string= val "x") (string-match "^x=" val))
      (setq val "x="))
     ((string-match "^\\$[\C-a-\C-z]" val)
      (setq val (concat "$^" (char-to-string (+ ?A -1 (aref val 1))))))
     ((string-match "^CORE::" val)
      (setq val "CORE::"))
     ((string-match "^SUPER::" val)
      (setq val "SUPER::"))
     ((and (string= "<" val) (string-match "^<\\$?[a-zA-Z0-9_:]+>" val))
      (setq val "<NAME>")))
    (setq regexp (concat "^"
			 "\\([^a-zA-Z0-9_:]+[ \t]+\\)?"
			 (regexp-quote val)
			 "\\([ \t([/]\\|$\\)"))

    ;; get the buffer with the documentation text
    (cperl-switch-to-doc-buffer)

    ;; lookup in the doc
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (list
       (if (re-search-forward regexp (point-max) t)
	   (save-excursion
	     (beginning-of-line 1)
	     (let ((lnstart (point)))
	       (end-of-line)
	       (message "%s" (buffer-substring lnstart (point)))))
	 (if cperl-message-on-help-error
	     (message "No definition for %s" val)))))))

(defvar cperl-short-docs 'please-ignore-this-line
  ;; Perl4 version was written by Johan Vromans (jvromans@squirrel.nl)
  "Short documentation of Perl functions and variables.
# based on \\='@(#)@ perl-descr.el 1.9 - describe-perl-symbol\\=' [Perl 5]
...	Range (list context); flip/flop [no flop when flip] (scalar context).
! ...	Logical negation.
... != ...	Numeric inequality.
... !~ ...	Search pattern, substitution, or translation (negated).
$!	In numeric context: errno.  In a string context: error string.
$\"	The separator which joins elements of arrays interpolated in strings.
$#	The output format for printed numbers.  Default is %.15g or close.
$$	Process number of this script.  Changes in the fork()ed child process.
$%	The current page number of the currently selected output channel.

	The following variables are always local to the current block:

$1	Match of the 1st set of parentheses in the last match (auto-local).
$2	Match of the 2nd set of parentheses in the last match (auto-local).
$3	Match of the 3rd set of parentheses in the last match (auto-local).
$4	Match of the 4th set of parentheses in the last match (auto-local).
$5	Match of the 5th set of parentheses in the last match (auto-local).
$6	Match of the 6th set of parentheses in the last match (auto-local).
$7	Match of the 7th set of parentheses in the last match (auto-local).
$8	Match of the 8th set of parentheses in the last match (auto-local).
$9	Match of the 9th set of parentheses in the last match (auto-local).
$&	The string matched by the last pattern match (auto-local).
$\\='	The string after what was matched by the last match (auto-local).
$\\=`	The string before what was matched by the last match (auto-local).

$(	The real gid of this process.
$)	The effective gid of this process.
$*	Deprecated: Set to 1 to do multiline matching within a string.
$+	The last bracket matched by the last search pattern.
$,	The output field separator for the print operator.
$-	The number of lines left on the page.
$.	The current input line number of the last filehandle that was read.
$/	The input record separator, newline by default.
$0	Name of the file containing the current perl script (read/write).
$:     String may be broken after these characters to fill ^-lines in a format.
$;	Subscript separator for multi-dim array emulation.  Default \"\\034\".
$<	The real uid of this process.
$=	The page length of the current output channel.  Default is 60 lines.
$>	The effective uid of this process.
$?	The status returned by the last \\=`\\=`, pipe close or `system'.
$@	The perl error message from the last eval or do @var{EXPR} command.
$ARGV	The name of the current file used with <> .
$[	Deprecated: The index of the first element/char in an array/string.
$\\	The output record separator for the print operator.
$]	The perl version string as displayed with perl -v.
$^	The name of the current top-of-page format.
$^A     The current value of the write() accumulator for format() lines.
$^D	The value of the perl debug (-D) flags.
$^E     Information about the last system error other than that provided by $!.
$^F	The highest system file descriptor, ordinarily 2.
$^H     The current set of syntax checks enabled by `use strict'.
$^I	The value of the in-place edit extension (perl -i option).
$^L     What formats output to perform a formfeed.  Default is \\f.
$^M     A buffer for emergency memory allocation when running out of memory.
$^O     The operating system name under which this copy of Perl was built.
$^P	Internal debugging flag.
$^T	The time the script was started.  Used by -A/-M/-C file tests.
$^W	True if warnings are requested (perl -w flag).
$^X	The name under which perl was invoked (argv[0] in C-speech).
$_	The default input and pattern-searching space.
$|	Auto-flush after write/print on current output channel?  Default 0.
$~	The name of the current report format.
... % ...	Modulo division.
... %= ...	Modulo division assignment.
%ENV	Contains the current environment.
%INC	List of files that have been require-d or do-ne.
%SIG	Used to set signal handlers for various signals.
... & ...	Bitwise and.
... && ...	Logical and.
... &&= ...	Logical and assignment.
... &= ...	Bitwise and assignment.
... * ...	Multiplication.
... ** ...	Exponentiation.
*NAME	Glob: all objects referred by NAME.  *NAM1 = *NAM2 aliases NAM1 to NAM2.
&NAME(arg0, ...)	Subroutine call.  Arguments go to @_.
... + ...	Addition.		+EXPR	Makes EXPR into scalar context.
++	Auto-increment (magical on strings).	++EXPR	EXPR++
... += ...	Addition assignment.
,	Comma operator.
... - ...	Subtraction.
--	Auto-decrement (NOT magical on strings).	--EXPR	EXPR--
... -= ...	Subtraction assignment.
-A	Access time in days since script started.
-B	File is a non-text (binary) file.
-C	Inode change time in days since script started.
-M	Age in days since script started.
-O	File is owned by real uid.
-R	File is readable by real uid.
-S	File is a socket .
-T	File is a text file.
-W	File is writable by real uid.
-X	File is executable by real uid.
-b	File is a block special file.
-c	File is a character special file.
-d	File is a directory.
-e	File exists .
-f	File is a plain file.
-g	File has setgid bit set.
-k	File has sticky bit set.
-l	File is a symbolic link.
-o	File is owned by effective uid.
-p	File is a named pipe (FIFO).
-r	File is readable by effective uid.
-s	File has non-zero size.
-t	Tests if filehandle (STDIN by default) is opened to a tty.
-u	File has setuid bit set.
-w	File is writable by effective uid.
-x	File is executable by effective uid.
-z	File has zero size.
.	Concatenate strings.
..	Range (list context); flip/flop (scalar context) operator.
.=	Concatenate assignment strings
... / ...	Division.	/PATTERN/ioxsmg	Pattern match
... /= ...	Division assignment.
/PATTERN/ioxsmg	Pattern match.
... < ...    Numeric less than.	<pattern>	Glob.	See <NAME>, <> as well.
<NAME>	Reads line from filehandle NAME (a bareword or dollar-bareword).
<pattern>	Glob (Unless pattern is bareword/dollar-bareword - see <NAME>).
<>	Reads line from union of files in @ARGV (= command line) and STDIN.
... << ...	Bitwise shift left.	<<	start of HERE-DOCUMENT.
... <= ...	Numeric less than or equal to.
... <=> ...	Numeric compare.
... = ...	Assignment.
... == ...	Numeric equality.
... =~ ...	Search pattern, substitution, or translation
... ~~ ..       Smart match
... > ...	Numeric greater than.
... >= ...	Numeric greater than or equal to.
... >> ...	Bitwise shift right.
... >>= ...	Bitwise shift right assignment.
... ? ... : ...	Condition=if-then-else operator.
... | ...	Bitwise or.
... || ...	Logical or.
... // ...      Defined-or.
~ ...		Unary bitwise complement.
... and ...		Low-precedence synonym for &&.
... cmp ...	String compare.
... eq ...	String equality.
... ge ...	String greater than or equal.
... gt ...	String greater than.
... le ...	String less than or equal.
... lt ...	String less than.
... ne ...	String inequality.
not ...		Low-precedence synonym for ! - negation.
... or ...		Low-precedence synonym for ||.
... x ...	Repeat string or array.
x= ...	Repetition assignment.
... xor ...		Low-precedence synonym for exclusive or.
@ARGV	Command line arguments (not including the command name - see $0).
@INC	List of places to look for perl scripts during do/include/use.
@_    Parameter array for subroutines; result of split() unless in list context.
\\  Creates reference to what follows, like \\$var.  Quotes non-\\w in strings.
\\0	Octal char, e.g. \\033.
\\E	Case modification terminator.  See \\Q, \\L, and \\U.
\\L	Lowercase until \\E .  See also \\l, lc.
\\U	Upcase until \\E .  See also \\u, uc.
\\Q	Quote metacharacters until \\E .  See also quotemeta.
\\a	Alarm character (octal 007).
\\b	Backspace character (octal 010).
\\c	Control character, e.g. \\c[ .
\\e	Escape character (octal 033).
\\f	Formfeed character (octal 014).
\\l	Lowercase the next character.  See also \\L and \\u, lcfirst.
\\n	Newline character (octal 012 on most systems).
\\r	Return character (octal 015 on most systems).
\\t	Tab character (octal 011).
\\u	Upcase the next character.  See also \\U and \\l, ucfirst.
\\x	Hex character, e.g. \\x1b.
... ^ ...	Bitwise exclusive or.
__CLASS__	The class of an object in construction
__DATA__	Ends program source.
__END__	Ends program source.
ADJUST {...}	Callback for object creation
ARGV	Default multi-file input filehandle.  <ARGV> is a synonym for <>.
ARGVOUT	Output filehandle with -i flag.
BEGIN { ... }	Immediately executed (during compilation) piece of code.
END { ... }	Pseudo-subroutine executed after the script finishes.
CHECK { ... }	Pseudo-subroutine executed after the script is compiled.
UNITCHECK { ... }
INIT { ... }	Pseudo-subroutine executed before the script starts running.
DATA	Input filehandle for what follows after __END__	or __DATA__.
abs [ EXPR ]	absolute value function
accept(NEWSOCKET,GENERICSOCKET)	accept an incoming socket connect
alarm(SECONDS)	schedule a SIGALRM
all {BLOCK} LIST	true if BLOCK is true for all elements in LIST
any {BLOCK} LIST	true if BLOCK is true for any element in LIST
async(SUB NAME {}|SUB {})	Mark function as potentially asynchronous
atan2(X,Y)	arctangent of Y/X in the range -PI to PI
await(ASYNCEXPR)	Yield result of Future
bind(SOCKET,NAME)	binds an address to a socket
binmode(FILEHANDLE)	prepare binary files for I/O
bless REFERENCE [, PACKAGE]	Makes reference into an object of a package.
break	Break out of a given/when statement
caller[(LEVEL)]	get context of the current subroutine call
chdir(EXPR)	change your current working directory
chmod(LIST)	changes the permissions on a list of files
chomp [LIST]	Strips $/ off LIST/$_.  Returns count.
chop[(LIST|VAR)]	remove the last character from a string
chown(LIST)	change the ownership on a list of files
chr [NUMBER]	Converts a number to char with the same ordinal.
chroot(FILENAME)	make directory new root for path lookups
class NAME	Introduce an object class.
close(FILEHANDLE)	close file (or pipe or socket) handle
closedir(DIRHANDLE)	close directory handle
connect(SOCKET,NAME)	connect to a remote socket
continue of { block } continue { block }.  Is executed after `next' or at end.
cos(EXPR)	cosine function
crypt(PLAINTEXT,SALT)	one-way passwd-style encryption
dbmclose(%HASH)	breaks binding on a tied dbm file
dbmopen(%HASH,DBNAME,MODE)	create binding on a tied dbm file
defined(EXPR)	test whether a value, variable, or function is defined
delete($HASH{KEY})	deletes a value from a hash
die(LIST)	raise an exception or bail out
do { ... }|SUBR while|until EXPR	executes at least once
do(EXPR|SUBR([LIST]))	(with while|until executes at least once)
dump LABEL	create an immediate core dump
each(%HASH)	retrieve the next key/value pair from a hash
endgrent	be done using group file
endhostent	be done using hosts file
endnetent	be done using networks file
endprotoent	be done using protocols file
endpwent	be done using passwd file
endservent	be done using services file
eof[([FILEHANDLE])]	test a filehandle for its end
eval(EXPR) or eval { BLOCK }	catch exceptions or compile and run code
evalbytes   See eval.
exec([TRUENAME] ARGV0, ARGVs)     or     exec(SHELL_COMMAND_LINE)
exists $HASH{KEY}	True if the key exists.
exit(EXPR)	terminate this program
exp(EXPR)	raise e to a power
fc EXPR    Returns the casefolded version of EXPR.
fcntl(FILEHANDLE,FUNCTION,SCALAR)
field VAR  [:param[(NAME)]] [=EXPR]	declare an object attribute
__FILE__	Current (source) filename.
fileno(FILEHANDLE)	return file descriptor from filehandle
flock(FILEHANDLE,OPERATION)	lock an entire file with an advisory lock
fork	create a new process just like this one
format [NAME] =	 Start of output format.  Ended by a single dot (.) on a line.
formline PICTURE, LIST	Backdoor into \"format\" processing.
getc[(FILEHANDLE)]	get the next character from the filehandle
getgrent	get group record given group user ID
getgrgid(GID)	get group record given group user ID
getgrnam(NAME)	get group record given group name
gethostbyaddr(ADDR,ADDRTYPE)	get host record given name
gethostbyname(NAME)	get host record given name
gethostent	get next hosts record
getlogin	return who logged in at this tty
getnetbyaddr(ADDR,ADDRTYPE)	get network record given its address
getnetbyname(NAME)	get networks record given name
getnetent	get next networks record
getpeername(SOCKET)	find the other end of a socket connection
getpgrp(PID)	get process group
getppid	get parent process ID
getpriority(WHICH,WHO)	get current nice value
getprotobyname(NAME)	get protocol record given name
getprotobynumber(NUMBER)	get protocol record numeric protocol
getprotoent	get next protocols record
getpwent	get next passwd record
getpwnam(NAME)	get passwd record given user login name
getpwuid(UID)	get passwd record given user ID
getservbyname(NAME,PROTO)	get services record given its name
getservbyport(PORT,PROTO)	get services record given numeric port
getservent	get next services record
getsockname(SOCKET)	retrieve the sockaddr for a given socket
getsockopt(SOCKET,LEVEL,OPTNAME)	get socket options on a given socket
glob EXPR	expand filenames using wildcards.  Synonym of <EXPR>.
gmtime(EXPR)	convert UNIX time into record or string using Greenwich time
goto LABEL	create spaghetti code
grep EXPR,LIST  or grep {BLOCK} LIST	Filters LIST via EXPR/BLOCK.
hex(EXPR)	convert a hexadecimal string to a number
import	patch a module's namespace into your own
index(STR,SUBSTR[,OFFSET])	find a substring within a string
int(EXPR)	get the integer portion of a number
ioctl(FILEHANDLE,FUNCTION,SCALAR)	device control system call
join(EXPR,LIST)	join a list into a string using a separator
keys(%HASH)	retrieve list of indices from a hash
kill(LIST)	send a signal to a process or process group
last [LABEL]	exit a block prematurely
lc [ EXPR ]	Returns lowercased EXPR.
lcfirst [ EXPR ]	Returns EXPR with lower-cased first letter.
length(EXPR)	return the number of characters in a string
__LINE__	Current line in current source.
link(OLDFILE,NEWFILE)	create a hard link in the filesystem
listen(SOCKET,QUEUESIZE)	register your socket as a server
local(LIST)	create a temporary value for a global variable
localtime(EXPR)	convert UNIX time into record or string using local time
lock(THING)	get a thread lock on a variable, subroutine, or method
log(EXPR)	retrieve the natural logarithm for a number
lstat(EXPR|FILEHANDLE|VAR)	stat a symbolic link
m/PATTERN/iogsmx	match a string with a regular expression pattern
map EXPR, LIST	or map {BLOCK} LIST	Applies EXPR/BLOCK to elts of LIST.
method  [NAME [(signature)]] { BODY }	method NAME;
mkdir(FILENAME,MODE)	create a directory
msgctl(ID,CMD,ARG)	SysV IPC message control operations
msgget(KEY,FLAGS)	get SysV IPC message queue
msgrcv(ID,VAR,SIZE,TYPE.FLAGS)	receive a SysV IPC message from a message queue
msgsnd(ID,MSG,FLAGS)	send a SysV IPC message to a message queue
my VAR or my (VAR1,...)	Introduces a lexical variable ($VAR, @ARR, or %HASH).
next [LABEL]	iterate a block prematurely
no MODULE [SYMBOL1, ...]  Partial reverse for `use'.  Runs `unimport' method.
oct(EXPR)	convert a string to an octal number
open(FILEHANDLE[,EXPR])	open a file, pipe, or descriptor
opendir(DIRHANDLE,EXPR)	open a directory
ord(EXPR)	ASCII value of the first char of the string.
our VAR or our (VAR1,...) Lexically enable a global variable ($V, @A, or %H).
pack(TEMPLATE,LIST)	convert a list into a binary representation
package NAME	Introduces package context.
__PACKAGE__	Current package.
pipe(READHANDLE,WRITEHANDLE)	Create a pair of filehandles on ends of a pipe.
pop(ARRAY)	remove the last element from an array and return it
pos STRING    Set/Get end-position of the last match over this string, see \\G.
print [FILEHANDLE] [(LIST)]	output a list to a filehandle
printf [FILEHANDLE] (FORMAT,LIST)	output a formatted list to a filehandle
prototype \\&SUB	Returns prototype of the function given a reference.
push(ARRAY,LIST)	append one or more elements to an array
q/STRING/	Synonym for \\='STRING\\='
qq/STRING/	Synonym for \"STRING\"
qr/PATTERN/	compile pattern
quotemeta	quote regular expression magic characters
qw/STRING/	quote a list of words
qx/STRING/	Synonym for \\=`STRING\\=`
rand[(EXPR)]	retrieve the next pseudorandom number
read(FILEHANDLE,SCALAR,LENGTH[,OFFSET])	fixed-length buffered input
readdir(DIRHANDLE)	get a directory from a directory handle
readline FH	Synonym of <FH>.
readlink(EXPR)	determine where a symbolic link is pointing
readpipe CMD	Synonym of \\=`CMD\\=`.
recv(SOCKET,SCALAR,LEN,FLAGS)	receive a message over a Socket
redo [LABEL]	start this loop iteration over again
ref [ EXPR ]	Type of EXPR when dereferenced.
rename(OLDNAME,NEWNAME)	change a filename
require [FILENAME | PERL_VERSION]	load from a library at runtime
reset[(EXPR)]	clear all variables of a given name
return(LIST)	get out of a function early
reverse(LIST)	flip a string or a list
rewinddir(DIRHANDLE)	reset directory handle
rindex(STR,SUBSTR[,OFFSET])	right-to-left substring search
rmdir(FILENAME)	remove a directory
s/PATTERN/REPLACEMENT/gieoxsm	replace a pattern with a string
say [FILEHANDLE] [(LIST)]	output a list, appending a newline
scalar(EXPR)	force a scalar context
seek(FILEHANDLE,POSITION,WHENCE)	reposition file pointer
seekdir(DIRHANDLE,POS)	reposition directory pointer
select(FILEHANDLE)	reset default output or do I/O multiplexing
select(RBITS,WBITS,EBITS,TIMEOUT)	do I/O multiplexing
semctl(ID,SEMNUM,CMD,ARG)	SysV semaphore control operations
semget(KEY,NSEMS,SIZE,FLAGS)	get set of SysV semaphores
semop(KEY,...)	SysV semaphore operations
send(SOCKET,MSG,FLAGS[,TO])	send a message over a socket
setgrent	prepare group file for use
sethostent(STAYOPEN)	prepare hosts file for use
setnetent(STAYOPEN)	prepare networks file for use
setpgrp(PID,PGRP)	set the process group of a process
setpriority(WHICH,WHO,PRIORITY)	Process	set a process\\='s nice value
setprotoent(STAYOPEN)	etwork	prepare protocols file for use
setpwent	prepare passwd file for use
setservent(STAYOPEN)	prepare services file for use
setsockopt(SOCKET,LEVEL,OPTNAME,OPTVAL)	set some socket options
shift[(ARRAY)]	remove the first element of an array, and return it
shmctl(ID,CMD,ARG)	SysV shared memory operations
shmget(KEY,SIZE,FLAGS)	get SysV shared memory segment identifier
shmread(ID,VAR,POS,SIZE)	read SysV shared memory
shmwrite(ID,STRING,POS,SIZE)	write SysV shared memory
shutdown(SOCKET,HOW)	close down just half of a socket connection
sin(EXPR)	return the sine of a number
sleep[(EXPR)]	block for some number of seconds
socket(SOCKET,DOMAIN,TYPE,PROTOCOL)	create a socket
socketpair(SOCKET1,SOCKET2,DOMAIN,TYPE,PROTOCOL)	create a pair of sockets
sort [SUBROUTINE] (LIST)	sort a list of values
splice(ARRAY,OFFSET[,LENGTH[,LIST]])	add or remove elements anywhere
split[(/PATTERN/[,EXPR[,LIMIT]])]	split up a string using a regexp
sprintf(FORMAT,LIST)	formatted print into a string
sqrt(EXPR)	square root function
srand(EXPR)	seed the random number generator
stat(EXPR|FILEHANDLE|VAR)	get a file\\='s status information
state VAR or state (VAR1,...)	Introduces a static lexical variable
study[(SCALAR)]	no-op, formerly optimized input data for repeated searches
sub [NAME [(format)]] { BODY }	sub NAME [(format)];	sub [(format)] {...}
__SUB__	the current subroutine, or C<undef> if not in a subroutine
substr(EXPR,OFFSET[,LEN])	get or alter a portion of a string
symlink(OLDFILE,NEWFILE)	create a symbolic link to a file
syscall(LIST)	execute an arbitrary system call
sysopen FH, FILENAME, MODE [, PERM]	(MODE is numeric, see Fcntl.)
sysread(FILEHANDLE,SCALAR,LENGTH[,OFFSET])	fixed-length unbuffered input
sysseek(FILEHANDLE,POSITION,WHENCE) position I/O pointer on handle
system([TRUENAME] ARGV0 [,ARGV])     or     system(SHELL_COMMAND_LINE)
syswrite(FILEHANDLE,SCALAR,LENGTH[,OFFSET])	fixed-length unbuffered output
tell[(FILEHANDLE)]	get current seekpointer on a filehandle
telldir(DIRHANDLE)	get current seekpointer on a directory handle
tie VAR, CLASS, LIST	Hide an object behind a simple Perl variable.
tied		Returns internal object for a tied data.
time	return number of seconds since 1970
times	return elapsed time for self and child processes
tr/SEARCHLIST/REPLACEMENTLIST/cds	transliterate a string
truncate(FILE|EXPR,LENGTH)	shorten a file
uc [ EXPR ]	Returns upcased EXPR.
ucfirst [ EXPR ]	Returns EXPR with upcased first letter.
umask[(EXPR)]	set file creation mode mask
undef[(EXPR)]	remove a variable or function definition
unlink(LIST)	remove one link to a file
unpack(TEMPLATE,EXPR)	convert binary structure into normal perl variables
unshift(ARRAY,LIST)	prepend more elements to the beginning of a list
untie VAR	Unlink an object from a simple Perl variable.
use MODULE [SYMBOL1, ...]  Compile-time `require' with consequent `import'.
utime(LIST)	set a file\\='s last access and modify times
values(%HASH)	return a list of the values in a hash
vec(EXPR,OFFSET,BITS)	test or set particular bits in a string
wait	wait for any child process to die
waitpid(PID,FLAGS)	wait for a particular child process to die
wantarray	Returns true if the sub/eval is called in list context.
warn(LIST)	print debugging info
write[(EXPR|FILEHANDLE)]	print a picture record
y/SEARCHLIST/REPLACEMENTLIST/	transliterate a string
#!	OS interpreter indicator.  If contains `perl', used for options, and -x.
AUTOLOAD {...}	Shorthand for `sub AUTOLOAD {...}'.
CORE::		Prefix to access builtin function if imported sub obscures it.
SUPER::		Prefix to lookup for a method in @ISA classes.
DESTROY		Shorthand for `sub DESTROY {...}'.
else		Part of if/unless {BLOCK} elsif {BLOCK} else {BLOCK}.
elsif		Part of if/unless {BLOCK} elsif {BLOCK} else {BLOCK}.
default { ... } default case for given/when block
defer { ... }	run this block after the containing block.
for (EXPR;EXPR;EXPR) { ... }
foreach [VAR] (@ARRAY) { ... }
given (EXPR) { [ when (EXPR) { ... } ]+ [ default { ... } ]? }
if (EXPR) { ... } [ elsif (EXPR) { ... } ... ] [ else { ... } ] or EXPR if EXPR
unless (EXPR) { ... } [ else { ... } ] or EXPR unless EXPR
until (EXPR) { ... }	EXPR until EXPR
while  (EXPR) { ... }	EXPR while EXPR
=head1		Top-level heading.
=head2		Second-level heading.
=head3		Third-level heading.
=head4		Fourth-level heading.
=over [ NUMBER ]	Start list.
=item [ TITLE ]		Start new item in the list.
=back		End list.
=cut		Switch from POD to Perl.
=pod		Switch from Perl to POD.
=begin formatname	Start directly formatted region.
=end formatname	End directly formatted region.
=for formatname text	Paragraph in special format.
=encoding encodingname	Encoding of the document.")

(defun cperl-switch-to-doc-buffer (&optional interactive)
  "Go to the Perl documentation buffer and insert the documentation.
If INTERACTIVE, open a new window for this buffer."
  (interactive "p")
  (let ((buf (get-buffer-create cperl-doc-buffer)))
    (if interactive
	(switch-to-buffer-other-window buf)
      (set-buffer buf))
    (if (= (buffer-size) 0)
	(progn
	  (insert (documentation-property 'cperl-short-docs
					  'variable-documentation))
	  (setq buffer-read-only t)))))

(defun cperl-beautify-regexp-piece (b e embed level)
  "Beautify part of a regexp from B to E.
If EMBED is nil, process the whole regular expression.  Recurse to depth
LEVEL."
  ;; b is before the starting delimiter, e before the ending
  ;; e should be a marker, may be changed, but remains "correct".
  ;; The REx is guaranteed to have //x
  ;; position at enter and at leave is not defined
  (let (s c tmp (m (make-marker)) (m1 (make-marker)) c1 spaces inline pos)
    (if embed
	(progn
	  (goto-char b)
	  (setq c (if (eq embed t) (current-indentation) (current-column)))
	  (cond ((looking-at "(\\?\\\\#") ; (?#) wrongly commented when //x-ing
		 (forward-char 2)
		 (delete-char 1)
		 (forward-char 1))
		((looking-at "(\\?[^a-zA-Z]")
		 (forward-char 3))
		((looking-at "(\\?")	; (?i)
		 (forward-char 2))
		(t
		 (forward-char 1))))
      (goto-char (1+ b))
      (setq c (1- (current-column))))
    (setq c1 (+ c (or cperl-regexp-indent-step cperl-indent-level)))
    (or (looking-at "[ \t]*[\n#]")
	(progn
	  (insert "\n")))
    (goto-char e)
    (beginning-of-line)
    (if (re-search-forward "[^ \t]" e t)
	(progn			       ; Something before the ending delimiter
	  (goto-char e)
	  (delete-horizontal-space)
	  (insert "\n")
	  (cperl-make-indent c)
	  (set-marker e (point))))
    (goto-char b)
    (end-of-line 2)
    (while (< (point) (marker-position e))
      (beginning-of-line)
      (setq s (point)
	    inline t)
      (skip-chars-forward " \t")
      (delete-region s (point))
      (cperl-make-indent c1)
      (while (and
	      inline
	      (looking-at
	       (concat "\\([a-zA-Z0-9]+[^*+{?]\\)" ; 1 word
		       "\\|"		; Embedded variable
		       "\\$\\([a-zA-Z0-9_]+\\([[{]\\)?\\|[^\n \t)|]\\)" ; 2 3
		       "\\|"		; $ ^
		       "[$^]"
		       "\\|"		; simple-code simple-code*?
		       "\\(\\\\.\\|[^][()#|*+?$^\n]\\)\\([*+{?]\\??\\)?" ; 4 5
		       "\\|"		; Class
		       "\\(\\[\\)"	; 6
		       "\\|"		; Grouping
		       "\\((\\(\\?\\)?\\)" ; 7 8
		       "\\|"		; |
		       "\\(|\\)")))	; 9
	(goto-char (match-end 0))
	(setq spaces t)
	(cond ((match-beginning 1)	; Alphanum word + junk
	       (forward-char -1))
	      ((or (match-beginning 3)	; $ab[12]
		   (and (match-beginning 5) ; X* X+ X{2,3}
			(eq (preceding-char) ?\{)))
	       (forward-char -1)
	       (forward-sexp 1))
	      ((and			; [], already syntaxified
		(match-beginning 6)
		cperl-regexp-scan
		cperl-use-syntax-table-text-property)
	       (forward-char -1)
	       (forward-sexp 1)
	       (or (eq (preceding-char) ?\])
		   (error "[]-group not terminated"))
	       (re-search-forward
		"\\=\\([*+?]\\|{[0-9]+\\(,[0-9]*\\)?}\\)\\??" e t))
	      ((match-beginning 6)	; []
	       (setq tmp (point))
	       (if (looking-at "\\^?\\]")
		   (goto-char (match-end 0)))
	       ;; XXXX POSIX classes?!
	       (while (and (not pos)
			   (re-search-forward "\\[:\\|\\]" e t))
		 (if (eq (preceding-char) ?:)
		     (or (re-search-forward ":\\]" e t)
			 (error "[:POSIX:]-group in []-group not terminated"))
		   (setq pos t)))
	       (or (eq (preceding-char) ?\])
		   (error "[]-group not terminated"))
	       (re-search-forward
		"\\=\\([*+?]\\|{[0-9]+\\(,[0-9]*\\)?}\\)\\??" e t))
	      ((match-beginning 7)	; ()
	       (goto-char (match-beginning 0))
	       (setq pos (current-column))
	       (or (eq pos c1)
		   (progn
		     (delete-horizontal-space)
		     (insert "\n")
		     (cperl-make-indent c1)))
	       (setq tmp (point))
	       (forward-sexp 1)
	       ;;	       (or (forward-sexp 1)
	       ;;		   (progn
	       ;;		     (goto-char tmp)
	       ;;		     (error "()-group not terminated")))
	       (set-marker m (1- (point)))
	       (set-marker m1 (point))
	       (if (= level 1)
		   (if (progn		; indent rigidly if multiline
			 ;; In fact does not make a lot of sense, since
			 ;; the starting position can be already lost due
			 ;; to insertion of "\n" and " "
			 (goto-char tmp)
			 (search-forward "\n" m1 t))
		       (indent-rigidly (point) m1 (- c1 pos)))
		 (setq level (1- level))
		 (cond
		  ((not (match-beginning 8))
		   (cperl-beautify-regexp-piece tmp m t level))
		  ((eq (char-after (+ 2 tmp)) ?\{) ; Code
		   t)
		  ((eq (char-after (+ 2 tmp)) ?\() ; Conditional
		   (goto-char (+ 2 tmp))
		   (forward-sexp 1)
		   (cperl-beautify-regexp-piece (point) m t level))
		  ((eq (char-after (+ 2 tmp)) ?<) ; Lookbehind
		   (goto-char (+ 3 tmp))
		   (cperl-beautify-regexp-piece (point) m t level))
		  (t
		   (cperl-beautify-regexp-piece tmp m t level))))
	       (goto-char m1)
	       (cond ((looking-at "[*+?]\\??")
		      (goto-char (match-end 0)))
		     ((eq (following-char) ?\{)
		      (forward-sexp 1)
		      (if (eq (following-char) ?\?)
			  (forward-char))))
	       (skip-chars-forward " \t")
	       (setq spaces nil)
	       (if (looking-at "[#\n]")
		   (progn
		     (or (eolp) (indent-for-comment))
		     (beginning-of-line 2))
		 (delete-horizontal-space)
		 (insert "\n"))
	       (end-of-line)
	       (setq inline nil))
	      ((match-beginning 9)	; |
	       (forward-char -1)
	       (setq tmp (point))
	       (beginning-of-line)
	       (if (re-search-forward "[^ \t]" tmp t)
		   (progn
		     (goto-char tmp)
		     (delete-horizontal-space)
		     (insert "\n"))
		 ;; first at line
		 (delete-region (point) tmp))
	       (cperl-make-indent c)
	       (forward-char 1)
	       (skip-chars-forward " \t")
	       (setq spaces nil)
	       (if (looking-at "[#\n]")
		   (beginning-of-line 2)
		 (delete-horizontal-space)
		 (insert "\n"))
	       (end-of-line)
	       (setq inline nil)))
	(or (looking-at "[ \t\n]")
	    (not spaces)
	    (insert " "))
	(skip-chars-forward " \t"))
      (or (looking-at "[#\n]")
	  (error "Unknown code `%s' in a regexp"
		 (buffer-substring (point) (1+ (point)))))
      (and inline (end-of-line 2)))
    ;; Special-case the last line of group
    (if (and (>= (point) (marker-position e))
	     (/= (current-indentation) c))
	(progn
	  (beginning-of-line)
	  (cperl-make-indent c)))))

(defun cperl-make-regexp-x ()
  "Ensure that a regular expression has a \"/x\" modifier."
  ;; Returns position of the start
  ;; XXX this is called too often!  Need to cache the result!
  (save-excursion
    (or cperl-use-syntax-table-text-property
	(error "I need to have a regexp marked!"))
    ;; Find the start
    (if (looking-at "\\s|")
	nil				; good already
      (if (or (looking-at "\\([smy]\\|qr\\)\\s|")
	      (and (eq (preceding-char) ?q)
		   (looking-at "\\(r\\)\\s|")))
	  (goto-char (match-end 1))
	(re-search-backward "\\s|")))	; Assume it is scanned already.
    ;;(forward-char 1)
    (let ((b (point)) (e (make-marker)) have-x delim
	  (sub-p (eq (preceding-char) ?s)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (setq delim (preceding-char))
      (if (and sub-p (eq delim (char-after (- (point) 2))))
	  (error "Possible s/blah// - do not know how to deal with"))
      (if sub-p (forward-sexp 1))
      (if (looking-at "\\sw*x")
	  (setq have-x t)
	(insert "x"))
      ;; Protect fragile " ", "#"
      (if have-x nil
	(goto-char (1+ b))
	(while (re-search-forward "\\(\\=\\|[^\\]\\)\\(\\\\\\\\\\)*[ \t\n#]" e t) ; Need to include (?#) too?
	  (forward-char -1)
	  (insert "\\")
	  (forward-char 1)))
      b)))

(defun cperl-beautify-regexp (&optional deep)
  "Beautify a regular expression to level DEEP.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive "P")
  (setq deep (if deep (prefix-numeric-value deep) -1))
  (save-excursion
    (goto-char (cperl-make-regexp-x))
    (let ((b (point)) (e (make-marker)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (cperl-beautify-regexp-piece b e nil deep))))

(defun cperl-regext-to-level-start ()
  "Goto start of an enclosing group in regexp.
We suppose that the regexp is scanned already."
  (interactive)
  (let ((limit (cperl-make-regexp-x)) done)
    (while (not done)
      (or (eq (following-char) ?\()
	  (search-backward "(" (1+ limit) t)
	  (error "Cannot find `(' which starts a group"))
      (setq done
	    (save-excursion
	      (skip-chars-backward "\\\\")
	      (looking-at "\\(\\\\\\\\\\)*(")))
      (or done (forward-char -1)))))

(defun cperl-contract-level ()
  "Find an enclosing group in regexp and contract it.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive)
  ;; (save-excursion		; Can't, breaks `cperl-contract-levels'
  (cperl-regext-to-level-start)
  (let ((b (point)) (e (make-marker)) c)
    (forward-sexp 1)
    (set-marker e (1- (point)))
    (goto-char b)
    (while (re-search-forward "\\(#\\)\\|\n" e 'to-end)
      (cond
       ((match-beginning 1)		; #-comment
	(or c (setq c (current-indentation)))
	(beginning-of-line 2)		; Skip
	(cperl-make-indent c))
       (t
	(delete-char -1)
	(just-one-space))))))

(defun cperl-contract-levels ()
  "Find an enclosing group in regexp and contract all the kids.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive)
  (save-excursion
    (condition-case nil
	(cperl-regext-to-level-start)
      (error				; We are outside outermost group
       (goto-char (cperl-make-regexp-x))))
    (let ((b (point)) (e (make-marker)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (goto-char (1+ b))
      (while (re-search-forward "\\(\\\\\\\\\\)\\|(" e t)
	(cond
	 ((match-beginning 1)		; Skip
	  nil)
	 (t				; Group
	  (cperl-contract-level)))))))

(defun cperl-beautify-level (&optional deep)
  "Find an enclosing group in regexp and beautify it.
Recurse to DEEP levels.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive "P")
  (setq deep (if deep (prefix-numeric-value deep) -1))
  (save-excursion
    (cperl-regext-to-level-start)
    (let ((b (point)) (e (make-marker)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (cperl-beautify-regexp-piece b e 'level deep))))

(defun cperl-invert-if-unless-modifiers ()
  "Change `B if A;' into `if (A) {B}' etc if possible.
\(Unfinished.)"
  (interactive)
  (let (A B pre-B post-B pre-if post-if pre-A post-A if-string
	  (w-rex "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>"))
    (and (= (char-syntax (preceding-char)) ?w)
	 (forward-sexp -1))
    (setq pre-if (point))
    (cperl-backward-to-start-of-expr)
    (setq pre-B (point))
    (forward-sexp 1)		; otherwise forward-to-end-of-expr is NOP
    (cperl-forward-to-end-of-expr)
    (setq post-A (point))
    (goto-char pre-if)
    (or (looking-at w-rex)
	;; Find the position
	(progn (goto-char post-A)
	       (while (and
		       (not (looking-at w-rex))
		       (> (point) pre-B))
		 (forward-sexp -1))
	       (setq pre-if (point))))
    (or (looking-at w-rex)
	(error "Can't find `if', `unless', `while', `until', `for' or `foreach'"))
    ;; 1 B 2 ... 3 B-com ... 4 if 5 ... if-com 6 ... 7 A 8
    (setq if-string (buffer-substring (match-beginning 0) (match-end 0)))
    ;; First, simple part: find code boundaries
    (forward-sexp 1)
    (setq post-if (point))
    (forward-sexp -2)
    (forward-sexp 1)
    (setq post-B (point))
    (cperl-backward-to-start-of-expr)
    (setq pre-B (point))
    (setq B (buffer-substring pre-B post-B))
    (goto-char pre-if)
    (forward-sexp 2)
    (forward-sexp -1)
    ;; May be after $, @, $# etc of a variable
    (skip-chars-backward "$@%#")
    (setq pre-A (point))
    (cperl-forward-to-end-of-expr)
    (setq post-A (point))
    (setq A (buffer-substring pre-A post-A))
    ;; Now modify (from end, to not break the stuff)
    (skip-chars-forward " \t;")
    (delete-region pre-A (point))	; we move to pre-A
    (insert "\n" B ";\n}")
    (and (looking-at "[ \t]*#") (cperl-indent-for-comment))
    (delete-region pre-if post-if)
    (delete-region pre-B post-B)
    (goto-char pre-B)
    (insert if-string " (" A ") {")
    (setq post-B (point))
    (if (looking-at "[ \t]+$")
	(delete-horizontal-space)
      (if (looking-at "[ \t]*#")
	  (cperl-indent-for-comment)
	(just-one-space)))
    (forward-line 1)
    (if (looking-at "[ \t]*$")
	(progn				; delete line
	  (delete-horizontal-space)
	  (delete-region (point) (1+ (point)))))
    (cperl-indent-line)
    (goto-char (1- post-B))
    (forward-sexp 1)
    (cperl-indent-line)
    (goto-char pre-B)))

(defun cperl-invert-if-unless ()
  "Change `if (A) {B}' into `B if A;' etc (or visa versa) if possible.
If the cursor is not on the leading keyword of the BLOCK flavor of
construct, will assume it is the STATEMENT flavor, so will try to find
the appropriate statement modifier."
  (interactive)
  (and (= (char-syntax (preceding-char)) ?w)
       (forward-sexp -1))
  (if (looking-at "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>")
      (let ((pre-if (point))
	    pre-A post-A pre-B post-B A B state p end-B-code is-block B-comment
	    (if-string (buffer-substring (match-beginning 0) (match-end 0))))
	(forward-sexp 2)
	(setq post-A (point))
	(forward-sexp -1)
	(setq pre-A (point))
	(setq is-block (and (eq (following-char) ?\( )
			    (save-excursion
			      (condition-case nil
				  (progn
				    (forward-sexp 2)
				    (forward-sexp -1)
				    (eq (following-char) ?\{ ))
				(error nil)))))
	(if is-block
	    (progn
	      (goto-char post-A)
	      (forward-sexp 1)
	      (setq post-B (point))
	      (forward-sexp -1)
	      (setq pre-B (point))
	      (if (and (eq (following-char) ?\{ )
		       (progn
			 (cperl-backward-to-noncomment post-A)
			 (eq (preceding-char) ?\) )))
		  (if (condition-case nil
			  (progn
			    (goto-char post-B)
			    (forward-sexp 1)
			    (forward-sexp -1)
			    (looking-at "\\<els\\(e\\|if\\)\\>"))
			(error nil))
		      (error
		       "`%s' (EXPR) {BLOCK} with `else'/`elsif'" if-string)
		    (goto-char (1- post-B))
		    (cperl-backward-to-noncomment pre-B)
		    (if (eq (preceding-char) ?\;)
			(forward-char -1))
		    (setq end-B-code (point))
		    (goto-char pre-B)
		    (while (re-search-forward "\\<\\(for\\|foreach\\|if\\|unless\\|while\\|until\\)\\>\\|;" end-B-code t)
		      (setq p (match-beginning 0)
			    A (buffer-substring p (match-end 0))
			    state (parse-partial-sexp pre-B p))
		      (or (nth 3 state)
			  (nth 4 state)
			  (nth 5 state)
			  (error "`%s' inside `%s' BLOCK" A if-string))
		      (goto-char (match-end 0)))
		    ;; Finally got it
		    (goto-char (1+ pre-B))
		    (skip-chars-forward " \t\n")
		    (setq B (buffer-substring (point) end-B-code))
		    (goto-char end-B-code)
		    (or (looking-at ";?[ \t\n]*}")
			(progn
			  (skip-chars-forward "; \t\n")
			  (setq B-comment
				(buffer-substring (point) (1- post-B)))))
		    (and (equal B "")
			 (setq B "1"))
		    (goto-char (1- post-A))
		    (cperl-backward-to-noncomment pre-A)
		    (or (looking-at "[ \t\n]*)")
			(goto-char (1- post-A)))
		    (setq p (point))
		    (goto-char (1+ pre-A))
		    (skip-chars-forward " \t\n")
		    (setq A (buffer-substring (point) p))
		    (delete-region pre-B post-B)
		    (delete-region pre-A post-A)
		    (goto-char pre-if)
		    (insert B " ")
		    (and B-comment (insert B-comment " "))
		    (just-one-space)
		    (forward-word-strictly 1)
		    (setq pre-A (point))
		    (insert " " A ";")
		    (delete-horizontal-space)
		    (setq post-B (point))
		    (if (looking-at "#")
			(indent-for-comment))
		    (goto-char post-B)
		    (forward-char -1)
		    (delete-horizontal-space)
		    (goto-char pre-A)
		    (just-one-space)
		    (goto-char pre-if)
		    (setq pre-A (set-marker (make-marker) pre-A))
		    (while (<= (point) (marker-position pre-A))
		      (cperl-indent-line)
		      (forward-line 1))
		    (goto-char (marker-position pre-A))
		    (if B-comment
			(progn
			  (forward-line -1)
			  (indent-for-comment)
			  (goto-char (marker-position pre-A)))))
		(error "`%s' (EXPR) not with an {BLOCK}" if-string)))
	  ;; (error "`%s' not with an (EXPR)" if-string)
	  (forward-sexp -1)
	  (cperl-invert-if-unless-modifiers)))
    ;;(error "Not at `if', `unless', `while', `until', `for' or `foreach'")
    (cperl-invert-if-unless-modifiers)))

(declare-function Man-getpage-in-background "man" (topic))

;; By Anthony Foiani <afoiani@uswest.com>
;; Getting help on modules in C-h f ?
;; This is a modified version of `man'.
;; Need to teach it how to lookup functions
;;;###autoload
(defun cperl-perldoc (word)
  "Run `perldoc' on WORD."
  (interactive
   (list (let* ((default-entry (cperl-word-at-point))
                (input (read-string
                        (cperl--format-prompt "perldoc entry" default-entry))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No perldoc args given")
                 default-entry)
             input))))
  (require 'man)
  (let* ((case-fold-search nil)
	 (is-func (and
		   (string-match "^\\(-[A-Za-z]\\|[a-z]+\\)$" word)
		   (string-match (concat "^" word "\\>")
				 (documentation-property
				  'cperl-short-docs
				  'variable-documentation))))
	 (Man-switches "")
         (manual-program (concat "perldoc -i" (if is-func " -f"))))
    (Man-getpage-in-background word)))

;;;###autoload
(defun cperl-perldoc-at-point ()
  "Run a `perldoc' on the word around point."
  (interactive)
  (cperl-perldoc (cperl-word-at-point)))

(define-obsolete-variable-alias 'pod2man-program 'cperl-pod2man-program "29.1")
(defcustom cperl-pod2man-program "pod2man"
  "File name for `pod2man'."
  :type 'file
  :group 'cperl
  :version "29.1")

;; By Nick Roberts <Nick.Roberts@src.bae.co.uk> (with changes)
(defun cperl-pod-to-manpage ()
  "Create a virtual manpage in Emacs from the Perl Online Documentation."
  (interactive)
  (require 'man)
  (let* ((pod2man-args (concat buffer-file-name " | nroff -man "))
	 (bufname (concat "Man " buffer-file-name))
	 (buffer (generate-new-buffer bufname)))
    (with-current-buffer buffer
      ;; Prevent any attempt to use display terminal fanciness.
      (with-environment-variables (("TERM" "dumb"))
        (set-process-sentinel
         (start-process pod2man-program buffer "sh" "-c"
                        (format (cperl-pod2man-build-command) pod2man-args))
         'Man-bgproc-sentinel)))))

(defun cperl-build-manpage ()
  "Create a virtual manpage in Emacs from the POD in the file."
  (interactive)
  (require 'man)
  (let ((manual-program "perldoc")
	(Man-switches ""))
    (Man-getpage-in-background buffer-file-name)))

(defun cperl-pod2man-build-command ()
  "Builds the entire background manpage and cleaning command."
  (let ((command (concat pod2man-program " %s 2>" null-device))
        (flist (and (boundp 'Man-filter-list) Man-filter-list)))
    (while (and flist (car flist))
      (let ((pcom (car (car flist)))
            (pargs (cdr (car flist))))
        (setq command
              (concat command " | " pcom " "
                      (mapconcat (lambda (phrase)
                                   (if (not (stringp phrase))
                                       (error "Malformed Man-filter-list"))
                                   phrase)
                                 pargs " ")))
        (setq flist (cdr flist))))
    command))


(defun cperl-next-interpolated-REx-1 ()
  "Move point to next REx which has interpolated parts without //o.
Skips RExes consisting of one interpolated variable.

Note that skipped RExen are not performance hits."
  (interactive "")
  (cperl-next-interpolated-REx 1))

(defun cperl-next-interpolated-REx-0 ()
  "Move point to next REx which has interpolated parts without //o."
  (interactive "")
  (cperl-next-interpolated-REx 0))

(defun cperl-next-interpolated-REx (&optional skip beg limit)
  "Move point to next REx which has interpolated parts.
SKIP is a list of possible types to skip, BEG and LIMIT are the starting
point and the limit of search (default to point and end of buffer).

SKIP may be a number, then it behaves as list of numbers up to SKIP; this
semantic may be used as a numeric argument.

Types are 0 for / $rex /o (interpolated once), 1 for /$rex/ (if $rex is
a result of qr//, this is not a performance hit), t for the rest."
  (interactive "P")
  (if (numberp skip) (setq skip (list 0 skip)))
  (or beg (setq beg (point)))
  (or limit (setq limit (point-max)))	; needed for n-s-p-c
  (let (pp)
    (and (eq (get-text-property beg 'syntax-type) 'string)
	 (setq beg (next-single-property-change beg 'syntax-type nil limit)))
    (cperl-map-pods-heres
     (lambda (s _e _p)
       (if (memq (get-text-property s 'REx-interpolated) skip)
           t
         (setq pp s)
         nil))	; nil stops
     'REx-interpolated beg limit)
    (if pp (goto-char pp)
      (message "No more interpolated REx"))))

;; Initial version contributed by Trey Belew
(defun cperl-here-doc-spell ()
  "Spell-check HERE-documents in the Perl buffer.
If a region is highlighted, restricts to the region."
  (interactive)
  (cperl-pod-spell t))

(defun cperl-pod-spell (&optional do-heres)
  "Spell-check POD documentation.  Do here-docs if DO-HERES.
If invoked with prefix argument, will do HERE-DOCs instead.
If a region is highlighted, restricts to the region."
  (interactive "P")
  (save-excursion
    (let (beg end)
      (if (region-active-p)
	  (setq beg (min (mark) (point))
		end (max (mark) (point)))
	(setq beg (point-min)
	      end (point-max)))
      (cperl-map-pods-heres (lambda (s e _p)
                         (if do-heres
                             (setq e (save-excursion
                                       (goto-char e)
                                       (forward-line -1)
                                       (point))))
                         (ispell-region s e)
                         t)
			    (if do-heres 'here-doc-group 'in-pod)
			    beg end))))

(defun cperl-map-pods-heres (func &optional prop s end)
  "Execute FUNC over regions of pods or here-documents.
Start at S.  PROP is the text-property to search for; default to
`in-pod'.  Stop at END or when FUNC returns nil."
  (let (pos posend has-prop (cont t))
    (or prop (setq prop 'in-pod))
    (or s (setq s (point-min)))
    (or end (setq end (point-max)))
    (cperl-update-syntaxification end)
    (save-excursion
      (goto-char (setq pos s))
      (while (and cont (< pos end))
	(setq has-prop (get-text-property pos prop))
	(setq posend (next-single-property-change pos prop nil end))
	(and has-prop
	     (setq cont (funcall func pos posend prop)))
	(setq pos posend)))))

;; Based on code by Masatake YAMATO:
(defun cperl-get-here-doc-region (&optional pos pod)
  "Return HERE document region around POS.
Return nil if the point is not in a HERE document region.  If POD is
non-nil, will return a POD section if point is in a POD section."
  (or pos (setq pos (point)))
  (cperl-update-syntaxification pos)
  (if (or (eq 'here-doc  (get-text-property pos 'syntax-type))
	  (and pod
	       (eq 'pod (get-text-property pos 'syntax-type))))
      (let ((b (cperl-beginning-of-property pos 'syntax-type))
	    (e (next-single-property-change pos 'syntax-type)))
	(cons b (or e (point-max))))))

(defun cperl-narrow-to-here-doc (&optional pos)
  "Narrows editing region to the HERE-DOC at POS.
POS defaults to the point."
  (interactive "d")
  (or pos (setq pos (point)))
  (let ((p (cperl-get-here-doc-region pos)))
    (or p (error "Not inside a HERE document"))
    (narrow-to-region (car p) (cdr p))
    (message (substitute-command-keys
              "When you are finished with narrow editing, type \\[widen]"))))

(defun cperl-select-this-pod-or-here-doc (&optional pos)
  "Select the HERE-DOC (or POD section) at POS.
POS defaults to the point."
  (interactive "d")
  (let ((p (cperl-get-here-doc-region pos t)))
    (if p
	(progn
	  (goto-char (car p))
	  (push-mark (cdr p) nil t))	; Message, activate in transient-mode
      (message "I do not think POS is in POD or a HERE-doc..."))))

(defun cperl-facemenu-add-face-function (face _end)
  "A callback to process user-initiated font-change requests.
Translates FACE which is one of `bold', `italic', and `bold-italic' to
insertion of corresponding POD directives, and `underline' to C<> POD
directive."
  (or (get-text-property (point) 'in-pod)
      (error "Faces can only be set within POD"))
  (setq facemenu-end-add-face (if (eq face 'bold-italic) ">>" ">"))
  (cdr (or (assq face '((bold . "B<")
			(italic . "I<")
			(bold-italic . "B<I<")
			(underline . "C<")))
	   (error "Face %S not configured for cperl-mode"
		  face))))

(defun cperl-time-fontification (&optional l step lim)
  "Times how long it takes to do incremental fontification in a region.
L is the line to start at, STEP is the number of lines to skip when
doing next incremental fontification, LIM is the maximal number of
incremental fontification to perform.  Messages are accumulated in
*Messages* buffer.

May be used for pinpointing which construct slows down buffer fontification:
start with default arguments, then refine the slowdown regions."
  (interactive "nLine to start at: \nnStep to do incremental fontification: ")
  (or l (setq l 1))
  (or step (setq step 500))
  (or lim (setq lim 40))
  (let* ((timems (lambda () (car (cperl--time-convert nil 1000))))
	 (tt (funcall timems)) (c 0) delta tot)
    (goto-char (point-min))
    (forward-line (1- l))
    (cperl-mode)
    (setq tot (- (- tt (setq tt (funcall timems)))))
    (message "cperl-mode at %s: %s" l tot)
    (while (and (< c lim) (not (eobp)))
      (forward-line step)
      (setq l (+ l step))
      (setq c (1+ c))
      (cperl-update-syntaxification (point))
      (setq delta (- (- tt (setq tt (funcall timems)))) tot (+ tot delta))
      (message "to %s:%6s,%7s" l delta tot))
    tot))

(defun cperl-emulate-lazy-lock (&optional window-size)
  "Emulate `lazy-lock' without `condition-case', so `debug-on-error' works.
Start fontifying the buffer from the start (or end) using the given
WINDOW-SIZE (units is lines).  Negative WINDOW-SIZE starts at end, and
goes backwards; default is -50.  This function is not CPerl-specific; it
may be used to debug problems with delayed incremental fontification."
  (interactive
   "nSize of window for incremental fontification, negative goes backwards: ")
  (or window-size (setq window-size -50))
  (let ((pos (if (> window-size 0)
		 (point-min)
	       (point-max)))
	p)
    (goto-char pos)
    (normal-mode)
    ;; Why needed???  With older font-locks???
    (setq-local font-lock-cache-position (make-marker))
    (while (if (> window-size 0)
	       (< pos (point-max))
	     (> pos (point-min)))
      (setq p (progn
		(forward-line window-size)
		(point)))
      (font-lock-fontify-region (min p pos) (max p pos))
      (setq pos p))))


(defvar cperl-help-shown nil
  "Non-nil means that the help was already shown now.")

(defvar cperl-lazy-installed nil
  "Non-nil means that the lazy-help handlers are installed now.")

;; FIXME: Use eldoc?
(defun cperl-lazy-install ()
  "Switch on Auto-Help on Perl constructs (put in the message area).
Delay of auto-help controlled by `cperl-lazy-help-time'."
  (interactive)
  (make-local-variable 'cperl-help-shown)
  (if (and (cperl-val 'cperl-lazy-help-time)
	   (not cperl-lazy-installed))
      (progn
	(add-hook 'post-command-hook #'cperl-lazy-hook)
	(run-with-idle-timer
	 (cperl-val 'cperl-lazy-help-time 1000000 5)
	 t
	 #'cperl-get-help-defer)
	(setq cperl-lazy-installed t))))

(defun cperl-lazy-unstall ()
  "Switch off Auto-Help on Perl constructs (put in the message area).
Delay of auto-help controlled by `cperl-lazy-help-time'."
  (interactive)
  (remove-hook 'post-command-hook #'cperl-lazy-hook)
  (cancel-function-timers #'cperl-get-help-defer)
  (setq cperl-lazy-installed nil))

(defun cperl-lazy-hook ()
  "Set display of cperl short-docs to be shown when idle."
  (setq cperl-help-shown nil))

(defun cperl-get-help-defer ()
  "Hook to display short-docs when idle."
  (if (not (memq major-mode '(perl-mode cperl-mode))) nil
    (let ((cperl-message-on-help-error nil) (cperl-help-from-timer t))
      (cperl-get-help)
      (setq cperl-help-shown t))))
(cperl-lazy-install)


;;; Plug for wrong font-lock:

(defun cperl-font-lock-unfontify-region-function (beg end)
  "Remove fontification between BEG and END."
  (declare (obsolete nil "30.1"))
  (with-silent-modifications
    (remove-text-properties beg end '(face nil))))

(defun cperl-font-lock-fontify-region-function (beg end loudly)
  "Extend the region to safe positions, then call the default function.
Process from BEG to END.  LOUDLY is passed to the default function.
Newer `font-lock's can do it themselves.
We unwind only as far as needed for fontification.  Syntaxification may
do extra unwind via `cperl-unwind-to-safe'."
  (save-excursion
    (goto-char beg)
    (while (and beg
		(progn
		  (beginning-of-line)
		  (eq (get-text-property (setq beg (point)) 'syntax-type)
		      'multiline)))
      (let ((new-beg (cperl-beginning-of-property beg 'syntax-type)))
	(setq beg (if (= new-beg beg) nil new-beg))
	(goto-char new-beg)))
    (setq beg (point))
    (goto-char end)
    (while (and end (< end (point-max))
		(progn
		  (or (bolp) (condition-case nil
				 (forward-line 1)
			       (error nil)))
		  (eq (get-text-property (setq end (point)) 'syntax-type)
		      'multiline)))
      (setq end (next-single-property-change end 'syntax-type nil (point-max)))
      (goto-char end))
    (setq end (point)))
  (font-lock-default-fontify-region beg end loudly))

(defun cperl-fontify-syntactically (end)
  "Find and apply text properties for \"hard\" syntax to END."
  ;; Some vars for debugging only
  ;; (message "Syntaxifying...")
  (let ((dbg (point)) (iend end) (idone cperl-syntax-done-to)
	(istate (car cperl-syntax-state))
	start from-start)
    (or cperl-syntax-done-to
	(setq cperl-syntax-done-to (point-min)
	      from-start t))
    (setq start (if (and cperl-hook-after-change
			 (not from-start))
		    cperl-syntax-done-to ; Fontify without change; ignore start
		  ;; Need to forget what is after `start'
		  (min cperl-syntax-done-to (point))))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (and cperl-syntaxify-unwind
	 (setq end (cperl-unwind-to-safe t end)
	       start (point)))
    (and (> end start)
	 (setq cperl-syntax-done-to start) ; In case what follows fails
	 (cperl-find-pods-heres start end t nil t))
    (if (memq cperl-syntaxify-by-font-lock '(backtrace message))
	(message "Syxify req=%s..%s actual=%s..%s done-to: %s=>%s statepos: %s=>%s"
		 dbg iend start end idone cperl-syntax-done-to
		 istate (car cperl-syntax-state))) ; For debugging
    nil))				; Do not iterate

(defun cperl-fontify-update (end)
  "Function to convert postponed fontification up to END to actual faces.
This function is part of our `font-lock-defaults'."
  (let ((pos (point-min)) prop posend)
    (setq end (point-max))
    (while (< pos end)
      (setq prop (get-text-property pos 'cperl-postpone)
	    posend (next-single-property-change pos 'cperl-postpone nil end))
      (and prop (put-text-property pos posend (car prop) (cdr prop)))
      (setq pos posend)))
  nil)					; Do not iterate

(defun cperl-fontify-update-bad (end)
  "Process postponed fontification to end of buffer, ignoring END.
This function is no longer needed."
  ;; Since fontification happens with different region than syntaxification,
  ;; do to the end of buffer, not to END
  ;; likewise, start earlier if needed
  (let* ((pos (point)) (prop (get-text-property pos 'cperl-postpone)) posend)
    (if prop
	(setq pos (or (cperl-beginning-of-property
		       (cperl-1+ pos) 'cperl-postpone)
		      (point-min))))
    (while (< pos end)
      (setq posend (next-single-property-change pos 'cperl-postpone))
      (and prop (put-text-property pos posend (car prop) (cdr prop)))
      (setq pos posend)
      (setq prop (get-text-property pos 'cperl-postpone))))
  nil)					; Do not iterate

;; Called when any modification is made to buffer text.
(defun cperl-after-change-function (beg _end _old-len)
  "Process information provided as an `after-change-function'.
Reset CPerl mode's syntax pointer to BEG."
  ;; We should have been informed about changes by `font-lock'.  Since it
  ;; does not inform as which calls are deferred, do it ourselves
  (if cperl-syntax-done-to
      (setq cperl-syntax-done-to (min cperl-syntax-done-to beg))))

(defun cperl-update-syntaxification (to)
  "Apply syntax table properties up to TO."
  (when cperl-use-syntax-table-text-property
    (syntax-propertize to)))

(defvar cperl-version
  (let ((v  "Revision: 6.2"))
    (string-match ":\\s *\\([0-9.]+\\)" v)
    (substring v (match-beginning 1) (match-end 1)))
  "Version of IZ-supported CPerl package this file is based on.")
(make-obsolete-variable 'cperl-version 'emacs-version "28.1")

(defvar cperl-do-not-fontify 'fontified
  "Text property which inhibits refontification.")
(make-obsolete-variable 'cperl-do-not-fontify nil "28.1")

;;; Minor mode for optional Perl features
(define-minor-mode cperl-extra-paired-delimiters-mode
  "Toggle treatment of extra paired delimiters in Perl.
Many non-ASCII paired delimiters can be used for quote-like constructs
by activating the feature \"extra_paired_delimiters\" either explicitly
or as part of the Perl 5.40 feature bundle.  This command allows
`cperl-mode' to recognize the same set of paired delimiters, see the
variable `cperl--extra-paired-delimiters'."
  :group 'cperl
  :lighter "«»"
  :interactive (cperl-mode)
  (if cperl-extra-paired-delimiters-mode
      (progn
        (setq-local cperl-starters cperl--extra-paired-delimiters)
        (cperl-find-pods-heres (point-min) (point-max)))
    (setq-local cperl-starters cperl--basic-paired-delimiters)
    (cperl-find-pods-heres (point-min) (point-max))))

(provide 'cperl-mode)

;;; cperl-mode.el ends here
