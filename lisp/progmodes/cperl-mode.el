;;; cperl-mode.el --- Perl code editing commands for Emacs

;; Copyright (C) 1985, 86, 87, 91, 92, 93, 94, 95, 96, 1997
;;     Free Software Foundation, Inc.

;; Author: Ilya Zakharevich and Bob Olson
;; Maintainer: Ilya Zakharevich <ilya@math.ohio-state.edu>
;; Keywords: languages, Perl

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

;;; Corrections made by Ilya Zakharevich ilya@math.mps.ohio-state.edu

;;; Commentary:

;;; You can either fine-tune the bells and whistles of this mode or
;;; bulk enable them by putting

;; (setq cperl-hairy t)

;;; in your .emacs file.  (Emacs rulers do not consider it politically
;;; correct to make whistles enabled by default.)

;;; DO NOT FORGET to read micro-docs (available from `Perl' menu)   <<<<<<
;;; or as help on variables `cperl-tips', `cperl-problems',         <<<<<<
;;; `cperl-non-problems', `cperl-praise', `cperl-speed'.            <<<<<<

;;; The mode information (on C-h m) provides some customization help.
;;; If you use font-lock feature of this mode, it is advisable to use
;;; either lazy-lock-mode or fast-lock-mode.  I prefer lazy-lock.

;;; Faces used now: three faces for first-class and second-class keywords
;;; and control flow words, one for each: comments, string, labels,
;;; functions definitions and packages, arrays, hashes, and variable
;;; definitions.  If you do not see all these faces, your font-lock does
;;; not define them, so you need to define them manually.

;;; into your .emacs file.

;;;; This mode supports font-lock, imenu and mode-compile.  In the
;;;; hairy version font-lock is on, but you should activate imenu
;;;; yourself (note that mode-compile is not standard yet).  Well, you
;;;; can use imenu from keyboard anyway (M-x imenu), but it is better
;;;; to bind it like that:

;; (define-key global-map [M-S-down-mouse-3] 'imenu)

;;; Code:

;; Some macros are needed for `defcustom'
(eval-when-compile
  (require 'font-lock)
  (defvar msb-menu-cond)
  (defvar gud-perldb-history)
  (defvar font-lock-background-mode)	; not in Emacs
  (defvar font-lock-display-type)	; ditto
  (defconst cperl-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version))
  (defmacro cperl-is-face (arg)		; Takes quoted arg
    (cond ((fboundp 'find-face)
	   `(find-face ,arg))
	  (;;(and (fboundp 'face-list)
	   ;;	(face-list))
	   (fboundp 'face-list)
	   `(member ,arg (and (fboundp 'face-list)
			      (face-list))))
	  (t
	   `(boundp ,arg))))
  (defmacro cperl-make-face (arg descr) ; Takes unquoted arg
    (cond ((fboundp 'make-face)
	   `(make-face (quote ,arg)))
	  (t
	   `(defconst ,arg (quote ,arg) ,descr))))
  (defmacro cperl-force-face (arg descr) ; Takes unquoted arg
    `(progn
       (or (cperl-is-face (quote ,arg))
	   (cperl-make-face ,arg ,descr))
       (or (boundp (quote ,arg))	; We use unquoted variants too
	   (defconst ,arg (quote ,arg) ,descr))))
  (if cperl-xemacs-p
      (defmacro cperl-etags-snarf-tag (file line)
	`(progn
	   (beginning-of-line 2)
	   (list ,file ,line)))
    (defmacro cperl-etags-snarf-tag (file line)
      `(etags-snarf-tag)))
  (if cperl-xemacs-p
      (defmacro cperl-etags-goto-tag-location (elt)
	;;(progn
	;; (switch-to-buffer (get-file-buffer (elt (, elt) 0)))
	;; (set-buffer (get-file-buffer (elt (, elt) 0)))
	;; Probably will not work due to some save-excursion???
	;; Or save-file-position?
	;; (message "Did I get to line %s?" (elt (, elt) 1))
	`(goto-line (string-to-int (elt ,elt 1))))
    ;;)
    (defmacro cperl-etags-goto-tag-location (elt)
      `(etags-goto-tag-location ,elt)))
  (autoload 'tmm-prompt "tmm"))

(defun cperl-choose-color (&rest list)
  (let (answer)
    (while list
      (or answer
	  (if (or (x-color-defined-p (car list))
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
  "Indentation."
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
  "*Non-nil means that if, elsif, while, until, else, for, foreach
and do constructs look like:

	if ()
	{
	}

instead of:

	if () {
	}
"
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-extra-newline-before-brace-multiline
  cperl-extra-newline-before-brace
  "*Non-nil means the same as `cperl-extra-newline-before-brace', but
for constructs with multiline if/unless/while/until/for/foreach condition."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-indent-level 2
  "*Indentation of CPerl statements with respect to containing block."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-lineup-step nil
  "*`cperl-lineup' will always lineup at multiple of this number.
If nil, the value of `cperl-indent-level' will be used."
  :type '(choice (const nil) integer)
  :group 'cperl-indentation-details)

(defcustom cperl-brace-imaginary-offset 0
  "*Imagined indentation of a Perl open brace that actually follows a statement.
An open brace following other text is treated as if it were this far
to the right of the start of its line."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-label-offset -2
  "*Offset of CPerl label lines relative to usual indentation."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-min-label-indent 1
  "*Minimal offset of CPerl label lines."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-continued-statement-offset 2
  "*Extra indent for lines not starting new statements."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to cperl-continued-statement-offset."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-close-paren-offset -1
  "*Extra indent for substatements that start with close-parenthesis."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in CPerl code.  The following
\\[cperl-electric-backspace] will remove the inserted whitespace.
Insertion after colons requires both this variable and
`cperl-auto-newline-after-colon' set."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-autoindent-on-semi nil
  "*Non-nil means automatically indent after insertion of (semi)colon.
Active if `cperl-auto-newline' is false."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-auto-newline-after-colon nil
  "*Non-nil means automatically newline even after colons.
Subject to `cperl-auto-newline' setting."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-tab-always-indent t
  "*Non-nil means TAB in CPerl mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-font-lock nil
  "*Non-nil (and non-null) means CPerl buffers will use `font-lock-mode'.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-lbrace-space nil
  "*Non-nil (and non-null) means { after $ should be preceded by ` '.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens-string "({[]})<"
  "*String of parentheses that should be electric in CPerl.
Closing ones are electric only if the region is highlighted."
  :type 'string
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens nil
  "*Non-nil (and non-null) means parentheses should be electric in CPerl.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defvar zmacs-regions)			; Avoid warning

(defcustom cperl-electric-parens-mark
  (and window-system
       (or (and (boundp 'transient-mark-mode) ; For Emacs
		transient-mark-mode)
	   (and (boundp 'zmacs-regions) ; For XEmacs
		zmacs-regions)))
  "*Not-nil means that electric parens look for active mark.
Default is yes if there is visual feedback on mark."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-electric-linefeed nil
  "*If true, LFD should be hairy in CPerl, otherwise C-c LFD is hairy.
In any case these two mean plain and hairy linefeeds together.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-keywords nil
  "*Not-nil (and non-null) means keywords are electric in CPerl.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-hairy nil
  "*Not-nil means most of the bells and whistles are enabled in CPerl.
Affects: `cperl-font-lock', `cperl-electric-lbrace-space',
`cperl-electric-parens', `cperl-electric-linefeed', `cperl-electric-keywords',
`cperl-info-on-command-no-prompt', `cperl-clobber-lisp-bindings',
`cperl-lazy-help-time'."
  :type 'boolean
  :group 'cperl-affected-by-hairy)

(defcustom cperl-comment-column 32
  "*Column to put comments in CPerl (use \\[cperl-indent] to lineup with code)."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-vc-header-alist '((SCCS "$sccs = '%W\%' ;")
				   (RCS "$rcs = ' $Id\$ ' ;"))
  "*What to use as `vc-header-alist' in CPerl."
  :type '(repeat (list symbol string))
  :group 'cperl)

(defcustom cperl-clobber-mode-lists
  (not
   (and
    (boundp 'interpreter-mode-alist)
    (assoc "miniperl" interpreter-mode-alist)
    (assoc "\\.\\([pP][Llm]\\|al\\)$" auto-mode-alist)))
  "*Whether to install us into `interpreter-' and `extension' mode lists."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-info-on-command-no-prompt nil
  "*Not-nil (and non-null) means not to prompt on C-h f.
The opposite behaviour is always available if prefixed with C-c.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-clobber-lisp-bindings nil
  "*Not-nil (and non-null) means not overwrite C-h f.
The function is available on \\[cperl-info-on-command], \\[cperl-get-help].
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-lazy-help-time nil
  "*Not-nil (and non-null) means to show lazy help after given idle time.
Can be overwritten by `cperl-hairy' to be 5 sec if nil."
  :type '(choice (const null) (const nil) integer)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-pod-face 'font-lock-comment-face
  "*Face for pod highlighting."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-pod-head-face 'font-lock-variable-name-face
  "*Face for pod highlighting.
Font for POD headers."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-here-face 'font-lock-string-face
  "*Face for here-docs highlighting."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-invalid-face 'underline
  "*Face for highlighting trailing whitespace."
  :type 'face
  :version "21.1"
  :group 'cperl-faces)

(defcustom cperl-pod-here-fontify '(featurep 'font-lock)
  "*Not-nil after evaluation means to highlight pod and here-docs sections."
  :type 'boolean
  :group 'cperl-faces)

(defcustom cperl-fontify-m-as-s t
  "*Not-nil means highlight 1arg regular expressions operators same as 2arg."
  :type 'boolean
  :group 'cperl-faces)

(defcustom cperl-highlight-variables-indiscriminately nil
  "*Non-nil means perform additional highlighting on variables.
Currently only changes how scalar variables are highlighted.
Note that that variable is only read at initialization time for
the variable `cperl-font-lock-keywords-2', so changing it after you've
entered `cperl-mode' the first time will have no effect."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-pod-here-scan t
  "*Not-nil means look for pod and here-docs sections during startup.
You can always make lookup from menu or using \\[cperl-find-pods-heres]."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-regexp-scan t
  "*Not-nil means make marking of regular expression more thorough.
Effective only with `cperl-pod-here-scan'.  Not implemented yet."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-imenu-addback nil
  "*Not-nil means add backreferences to generated `imenu's.
May require patched `imenu' and `imenu-go'.  Obsolete."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-max-help-size 66
  "*Non-nil means shrink-wrapping of info-buffer allowed up to these percents."
  :type '(choice integer (const nil))
  :group 'cperl-help-system)

(defcustom cperl-shrink-wrap-info-frame t
  "*Non-nil means shrink-wrapping of info-buffer-frame allowed."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-info-page "perl"
  "*Name of the info page containing perl docs.
Older version of this page was called `perl5', newer `perl'."
  :type 'string
  :group 'cperl-help-system)

(defcustom cperl-use-syntax-table-text-property
  (boundp 'parse-sexp-lookup-properties)
  "*Non-nil means CPerl sets up and uses `syntax-table' text property."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-use-syntax-table-text-property-for-tags
  cperl-use-syntax-table-text-property
  "*Non-nil means: set up and use `syntax-table' text property generating TAGS."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-scan-files-regexp "\\.\\([pP][Llm]\\|xs\\)$"
  "*Regexp to match files to scan when generating TAGS."
  :type 'regexp
  :group 'cperl)

(defcustom cperl-noscan-files-regexp "/\\(\\.\\.?\\|SCCS\\|RCS\\|blib\\)$"
  "*Regexp to match files/dirs to skip when generating TAGS."
  :type 'regexp
  :group 'cperl)

(defcustom cperl-regexp-indent-step nil
  "*Indentation used when beautifying regexps.
If nil, the value of `cperl-indent-level' will be used."
  :type '(choice integer (const nil))
  :group 'cperl-indentation-details)

(defcustom cperl-indent-left-aligned-comments t
  "*Non-nil means that the comment starting in leftmost column should indent."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-under-as-char nil
  "*Non-nil means that the _ (underline) should be treated as word char."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-extra-perl-args ""
  "*Extra arguments to use when starting Perl.
Currently used with `cperl-check-syntax' only."
  :type 'string
  :group 'cperl)

(defcustom cperl-message-electric-keyword t
  "*Non-nil means that the `cperl-electric-keyword' prints a help message."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-indent-region-fix-constructs 1
  "*Amount of space to insert between `}' and `else' or `elsif'
in `cperl-indent-region'.  Set to nil to leave as is.  Values other
than 1 and nil will probably not work."
  :type '(choice (const nil) (const 1))
  :group 'cperl-indentation-details)

(defcustom cperl-break-one-line-blocks-when-indent t
  "*Non-nil means that one-line if/unless/while/until/for/foreach BLOCKs
need to be reformated into multiline ones when indenting a region."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-fix-hanging-brace-when-indent t
  "*Non-nil means that BLOCK-end `}' may be put on a separate line
when indenting a region.
Braces followed by else/elsif/while/until are excepted."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-merge-trailing-else t
  "*Non-nil means that BLOCK-end `}' followed by else/elsif/continue
may be merged to be on the same line when indenting a region."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-indent-parens-as-block nil
  "*Non-nil means that non-block ()-, {}- and []-groups are indented as blocks,
but for trailing \",\" inside the group, which won't increase indentation.
One should tune up `cperl-close-paren-offset' as well."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-syntaxify-by-font-lock 
  (and window-system 
       (boundp 'parse-sexp-lookup-properties))
  "*Non-nil means that CPerl uses `font-lock's routines for syntaxification."
  :type '(choice (const message) boolean)
  :group 'cperl-speed)

(defcustom cperl-syntaxify-unwind
  t
  "*Non-nil means that CPerl unwinds to a start of along construction
when syntaxifying a chunk of buffer."
  :type 'boolean
  :group 'cperl-speed)

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
    (underline				nil "LightGray"	strikeout))
  "List given as an argument to `ps-extend-face-list' in `cperl-ps-print'."
  :type '(repeat (cons symbol
		       (cons (choice (const nil) string)
			     (cons (choice (const nil) string)
				   (repeat symbol)))))
  :group 'cperl-faces)

(if window-system
    (progn
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
	"Font Lock mode face used to highlight array names."
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
	:group 'cperl-faces)))



;;; Short extra-docs.

(defvar cperl-tips 'please-ignore-this-line
  "Get newest version of this package from
  ftp://ftp.math.ohio-state.edu/pub/users/ilya/emacs
and/or
  ftp://ftp.math.ohio-state.edu/pub/users/ilya/perl
Subdirectory `cperl-mode' may contain yet newer development releases and/or
patches to related files.

For best results apply to an older Emacs the patches from
  ftp://ftp.math.ohio-state.edu/pub/users/ilya/cperl-mode/patches
\(this upgrades syntax-parsing abilities of Emaxen v19.34 and
v20.2 up to the level of Emacs v20.3 - a must for a good Perl
mode.)

Get support packages choose-color.el (or font-lock-extra.el before
19.30), imenu-go.el from the same place.  \(Look for other files there
too... ;-).  Get a patch for imenu.el in 19.29.  Note that for 19.30 and
later you should use choose-color.el *instead* of font-lock-extra.el
\(and you will not get smart highlighting in C :-().

Note that to enable Compile choices in the menu you need to install
mode-compile.el.

If your Emacs does not default to `cperl-mode' on Perl files, and you
want it to: put the following into your .emacs file:

  (defalias 'perl-mode 'cperl-mode)

Get perl5-info from 
  $CPAN/doc/manual/info/perl-info.tar.gz
older version was on
  http://www.metronet.com:70/9/perlinfo/perl5/manual/perl5-info.tar.gz

If you use imenu-go, run imenu on perl5-info buffer (you can do it
from Perl menu).  If many files are related, generate TAGS files from
Tools/Tags submenu in Perl menu.

If some class structure is too complicated, use Tools/Hierarchy-view
from Perl menu, or hierarchic view of imenu.  The second one uses the
current buffer only, the first one requires generation of TAGS from
Perl/Tools/Tags menu beforehand.

Run Perl/Tools/Insert-spaces-if-needed to fix your lazy typing.

Switch auto-help on/off with Perl/Tools/Auto-help.

Though with contemporary Emaxen CPerl mode should maintain the correct
parsing of Perl even when editing, sometimes it may be lost.  Fix this by

  \\[normal-mode]

In cases of more severe confusion sometimes it is helpful to do

  \\[load-library] cperl-mode RET
  \\[normal-mode]

Before reporting (non-)problems look in the problem section of online
micro-docs on what I know about CPerl problems.")

(defvar cperl-problems 'please-ignore-this-line
"Some faces will not be shown on some versions of Emacs unless you
install choose-color.el, available from
   ftp://ftp.math.ohio-state.edu/pub/users/ilya/emacs/

`fill-paragraph' on a comment may leave the point behind the
paragraph.  Parsing of lines with several <<EOF is not implemented
yet.

Emacs had a _very_ restricted syntax parsing engine until version
20.1.  Most problems below are corrected starting from this version of
Emacs, and all of them should be fixed in version 20.3.  (Or apply
patches to Emacs 19.33/34 - see tips.)

Note that even with newer Emacsen in some very rare cases the details
of interaction of `font-lock' and syntaxification may be not cleaned
up yet.  You may get slightly different colors basing on the order of
fontification and syntaxification.  Say, the initial faces is correct,
but editing the buffer breaks this.

Even with older Emacsen CPerl mode tries to corrects some Emacs
misunderstandings, however, for efficiency reasons the degree of
correction is different for different operations.  The partially
corrected problems are: POD sections, here-documents, regexps.  The
operations are: highlighting, indentation, electric keywords, electric
braces.

This may be confusing, since the regexp s#//#/#\; may be highlighted
as a comment, but it will be recognized as a regexp by the indentation
code.  Or the opposite case, when a pod section is highlighted, but
may break the indentation of the following code (though indentation
should work if the balance of delimiters is not broken by POD).

The main trick (to make $ a \"backslash\") makes constructions like
${aaa} look like unbalanced braces.  The only trick I can think of is
to insert it as $ {aaa} (legal in perl5, not in perl4).

Similar problems arise in regexps, when /(\\s|$)/ should be rewritten
as /($|\\s)/.  Note that such a transposition is not always possible.

The solution is to upgrade your Emacs or patch an older one.  Note
that Emacs 20.2 has some bugs related to `syntax-table' text
properties.  Patches are available on the main CPerl download site,
and on CPAN.

If these bugs cannot be fixed on your machine (say, you have an inferior
environment and cannot recompile), you may still disable all the fancy stuff
via `cperl-use-syntax-table-text-property'." )

(defvar cperl-non-problems 'please-ignore-this-line
"As you know from `problems' section, Perl syntax is too hard for CPerl on
older Emacsen.  Here is what you can do if you cannot upgrade, or if
you want to switch off these capabilities on Emacs 20.2 (+patches) or 20.3
or better.  Please skip this docs if you run a capable Emacs already.

Most of the time, if you write your own code, you may find an equivalent
\(and almost as readable) expression (what is discussed below is usually
not relevant on newer Emacsen, since they can do it automatically).

Try to help CPerl: add comments with embedded quotes to fix CPerl
misunderstandings about the end of quotation:

$a='500$';      # ';

You won't need it too often.  The reason: $ \"quotes\" the following
character (this saves a life a lot of times in CPerl), thus due to
Emacs parsing rules it does not consider tick (i.e., ' ) after a
dollar as a closing one, but as a usual character.  This is usually
correct, but not in the above context.

Even with older Emacsen the indentation code is pretty wise.  The only
drawback is that it relied on Emacs parsing to find matching
parentheses.  And Emacs *could not* match parentheses in Perl 100%
correctly.  So
	1 if s#//#/#;
would not break indentation, but
	1 if ( s#//#/# );
would.  Upgrade.

By similar reasons
	s\"abc\"def\";
could confuse CPerl a lot.

If you still get wrong indentation in situation that you think the
code should be able to parse, try:

a) Check what Emacs thinks about balance of your parentheses.
b) Supply the code to me (IZ).

Pods were treated _very_ rudimentally.  Here-documents were not
treated at all (except highlighting and inhibiting indentation).  Upgrade.

To speed up coloring the following compromises exist:
   a) sub in $mypackage::sub may be highlighted.
   b) -z in [a-z] may be highlighted.
   c) if your regexp contains a keyword (like \"s\"), it may be highlighted.


Imenu in 19.31 is broken.  Set `imenu-use-keymap-menu' to t, and remove
`car' before `imenu-choose-buffer-index' in `imenu'.
`imenu-add-to-menubar' in 20.2 is broken.

A lot of things on XEmacs may be broken too, judging by bug reports I
receive.  Note that some releases of XEmacs are better than the others
as far as bugs reports I see are concerned.")

(defvar cperl-praise 'please-ignore-this-line
  "Advantages of CPerl mode.

0) It uses the newest `syntax-table' property ;-);

1) It does 99% of Perl syntax correct (as opposed to 80-90% in Perl
mode - but the latter number may have improved too in last years) even
with old Emaxen which do not support `syntax-table' property.

When using `syntax-table' property for syntax assist hints, it should
handle 99.995% of lines correct - or somesuch.  It automatically
updates syntax assist hints when you edit your script.

2) It is generally believed to be \"the most user-friendly Emacs
package\" whatever it may mean (I doubt that the people who say similar
things tried _all_ the rest of Emacs ;-), but this was not a lonely
voice);

3) Everything is customizable, one-by-one or in a big sweep;

4) It has many easily-accessable \"tools\":
        a) Can run program, check syntax, start debugger;
        b) Can lineup vertically \"middles\" of rows, like `=' in
                a  = b;
                cc = d;
        c) Can insert spaces where this impoves readability (in one
                interactive sweep over the buffer);
        d) Has support for imenu, including:
                1) Separate unordered list of \"interesting places\";
                2) Separate TOC of POD sections;
                3) Separate list of packages;
                4) Hierarchical view of methods in (sub)packages;
                5) and functions (by the full name - with package);
        e) Has an interface to INFO docs for Perl; The interface is
                very flexible, including shrink-wrapping of
                documentation buffer/frame;
        f) Has a builtin list of one-line explanations for perl constructs.
        g) Can show these explanations if you stay long enough at the
                corresponding place (or on demand);
        h) Has an enhanced fontification (using 3 or 4 additional faces
                comparing to font-lock - basically, different
                namespaces in Perl have different colors);
        i) Can construct TAGS basing on its knowledge of Perl syntax,
                the standard menu has 6 different way to generate
                TAGS (if \"by directory\", .xs files - with C-language
                bindings - are included in the scan);
        j) Can build a hierarchical view of classes (via imenu) basing
                on generated TAGS file;
        k) Has electric parentheses, electric newlines, uses Abbrev
                for electric logical constructs
                        while () {}
                with different styles of expansion (context sensitive
                to be not so bothering).  Electric parentheses behave
                \"as they should\" in a presence of a visible region.
        l) Changes msb.el \"on the fly\" to insert a group \"Perl files\";
        m) Can convert from
		if (A) { B }
	   to
		B if A;

        n) Highlights (by user-choice) either 3-delimiters constructs
	   (such as tr/a/b/), or regular expressions and `y/tr';
	o) Highlights trailing whitespace;
	p) Is able to manipulate Perl Regular Expressions to ease
	   conversion to a more readable form.

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

10) Uses a linear-time algorith for indentation of regions (on Emaxen with
capable syntax engines).
")

(defvar cperl-speed 'please-ignore-this-line
  "This is an incomplete compendium of what is available in other parts
of CPerl documentation.  (Please inform me if I skept anything.)

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
    A2) On capable Emaxen CPerl will use improved syntax-handlings
	which reads mark-up hints directly.

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
    of, say, long POD sections.
")

(defvar cperl-tips-faces 'please-ignore-this-line
  "CPerl mode uses following faces for highlighting:

  `cperl-array-face'		Array names
  `cperl-hash-face'		Hash names
  `font-lock-comment-face'	Comments, PODs and whatever is considered
				syntaxically to be not code
  `font-lock-constant-face'	HERE-doc delimiters, labels, delimiters of
				2-arg operators s/y/tr/ or of RExen,
  `font-lock-function-name-face' Special-cased m// and s//foo/, _ as 
				a target of a file tests, file tests,
				subroutine names at the moment of definition
				(except those conflicting with Perl operators),
				package names (when recognized), format names
  `font-lock-keyword-face'	Control flow switch constructs, declarators
  `cperl-nonoverridable-face'	Non-overridable keywords, modifiers of RExen
  `font-lock-string-face'	Strings, qw() constructs, RExen, POD sections,
				literal parts and the terminator of formats
				and whatever is syntaxically considered
				as string literals
  `font-lock-type-face'		Overridable keywords
  `font-lock-variable-name-face' Variable declarations, indirect array and
				hash names, POD headers/item names
  `cperl-invalid-face'		Trailing whitespace

Note that in several situations the highlighting tries to inform about
possible confusion, such as different colors for function names in
declarations depending on what they (do not) override, or special cases
m// and s/// which do not do what one would expect them to do.

Help with best setup of these faces for printout requested (for each of
the faces: please specify bold, italic, underline, shadow and box.)

\(Not finished.)")



;;; Portability stuff:

(defconst cperl-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version))

(defmacro cperl-define-key (emacs-key definition &optional xemacs-key)
  `(define-key cperl-mode-map
     ,(if xemacs-key
	  `(if cperl-xemacs-p ,xemacs-key ,emacs-key)
	emacs-key)
     ,definition))

(defvar cperl-del-back-ch
  (car (append (where-is-internal 'delete-backward-char)
	       (where-is-internal 'backward-delete-char-untabify)))
  "Character generated by key bound to `delete-backward-char'.")

(and (vectorp cperl-del-back-ch) (= (length cperl-del-back-ch) 1)
     (setq cperl-del-back-ch (aref cperl-del-back-ch 0)))

(defun cperl-mark-active () (mark))	; Avoid undefined warning
(if cperl-xemacs-p
    (progn
      ;; "Active regions" are on: use region only if active
      ;; "Active regions" are off: use region unconditionally
      (defun cperl-use-region-p ()
	(if zmacs-regions (mark) t)))
  (defun cperl-use-region-p ()
    (if transient-mark-mode mark-active t))
  (defun cperl-mark-active () mark-active))

(defsubst cperl-enable-font-lock ()
  (or cperl-xemacs-p window-system))

(defun cperl-putback-char (c)	; Emacs 19
  (set 'unread-command-events (list c))) ; Avoid undefined warning

(if (boundp 'unread-command-events)
    (if cperl-xemacs-p
	(defun cperl-putback-char (c)	; XEmacs >= 19.12
	  (setq unread-command-events (list (eval '(character-to-event c))))))
  (defun cperl-putback-char (c)		; XEmacs <= 19.11
    (set 'unread-command-event (eval '(character-to-event c))))) ; Avoid warnings

(or (fboundp 'uncomment-region)
    (defun uncomment-region (beg end)
      (interactive "r")
      (comment-region beg end -1)))

(defvar cperl-do-not-fontify
  (if (string< emacs-version "19.30")
      'fontified
    'lazy-lock)
  "Text property which inhibits refontification.")

(defsubst cperl-put-do-not-fontify (from to &optional post)
  ;; If POST, do not do it with postponed fontification
  (if (and post cperl-syntaxify-by-font-lock)
      nil
  (put-text-property (max (point-min) (1- from))
		       to cperl-do-not-fontify t)))

(defcustom cperl-mode-hook nil
  "Hook run by `cperl-mode'."
  :type 'hook
  :group 'cperl)

(defvar cperl-syntax-state nil)
(defvar cperl-syntax-done-to nil)
(defvar cperl-emacs-can-parse (> (length (save-excursion
					   (parse-partial-sexp 1 1))) 9))

;; Make customization possible "in reverse"
(defsubst cperl-val (symbol &optional default hairy)
  (cond
   ((eq (symbol-value symbol) 'null) default)
   (cperl-hairy (or hairy t))
   (t (symbol-value symbol))))

;;; Probably it is too late to set these guys already, but it can help later:

;;;(and cperl-clobber-mode-lists
;;;(setq auto-mode-alist
;;;      (append '(("\\.\\([pP][Llm]\\|al\\)$" . perl-mode))  auto-mode-alist ))
;;;(and (boundp 'interpreter-mode-alist)
;;;     (setq interpreter-mode-alist (append interpreter-mode-alist
;;;					  '(("miniperl" . perl-mode))))))
(eval-when-compile
  (condition-case nil
      (require 'imenu)
    (error nil))
  (condition-case nil
      (require 'easymenu)
    (error nil))
  (condition-case nil
      (require 'etags)
    (error nil))
  (condition-case nil
      (require 'timer)
    (error nil))
  (condition-case nil
      (require 'man)
    (error nil))
  (condition-case nil
      (require 'info)
    (error nil))
  (if (fboundp 'ps-extend-face-list)
      (defmacro cperl-ps-extend-face-list (arg)
	`(ps-extend-face-list ,arg))
    (defmacro cperl-ps-extend-face-list (arg)
      `(error "This version of Emacs has no `ps-extend-face-list'")))
  ;; Calling `cperl-enable-font-lock' below doesn't compile on XEmacs,
  ;; macros instead of defsubsts don't work on Emacs, so we do the
  ;; expansion manually.  Any other suggestions?
  (require 'cl))

(defvar cperl-mode-abbrev-table nil
  "Abbrev table in use in Cperl-mode buffers.")

(add-hook 'edit-var-mode-alist '(perl-mode (regexp . "^cperl-")))

(defvar cperl-mode-map () "Keymap used in CPerl mode.")

(if cperl-mode-map nil
  (setq cperl-mode-map (make-sparse-keymap))
  (cperl-define-key "{" 'cperl-electric-lbrace)
  (cperl-define-key "[" 'cperl-electric-paren)
  (cperl-define-key "(" 'cperl-electric-paren)
  (cperl-define-key "<" 'cperl-electric-paren)
  (cperl-define-key "}" 'cperl-electric-brace)
  (cperl-define-key "]" 'cperl-electric-rparen)
  (cperl-define-key ")" 'cperl-electric-rparen)
  (cperl-define-key ";" 'cperl-electric-semi)
  (cperl-define-key ":" 'cperl-electric-terminator)
  (cperl-define-key "\C-j" 'newline-and-indent)
  (cperl-define-key "\C-c\C-j" 'cperl-linefeed)
  (cperl-define-key "\C-c\C-t" 'cperl-invert-if-unless)
  (cperl-define-key "\C-c\C-a" 'cperl-toggle-auto-newline)
  (cperl-define-key "\C-c\C-k" 'cperl-toggle-abbrev)
  (cperl-define-key "\C-c\C-w" 'cperl-toggle-construct-fix)
  (cperl-define-key "\C-c\C-f" 'auto-fill-mode)
  (cperl-define-key "\C-c\C-e" 'cperl-toggle-electric)
  (cperl-define-key "\C-c\C-ha" 'cperl-toggle-autohelp)
  (cperl-define-key "\e\C-q" 'cperl-indent-exp) ; Usually not bound
  (cperl-define-key [?\C-\M-\|] 'cperl-lineup
		    [(control meta |)])
  ;;(cperl-define-key "\M-q" 'cperl-fill-paragraph)
  ;;(cperl-define-key "\e;" 'cperl-indent-for-comment)
  (cperl-define-key "\177" 'cperl-electric-backspace)
  (cperl-define-key "\t" 'cperl-indent-command)
  ;; don't clobber the backspace binding:
  (cperl-define-key "\C-c\C-hF" 'cperl-info-on-command
		    [(control c) (control h) F])
  (if (cperl-val 'cperl-clobber-lisp-bindings)
      (progn
	(cperl-define-key "\C-hf"
			  ;;(concat (char-to-string help-char) "f") ; does not work
			  'cperl-info-on-command
			  [(control h) f])
	(cperl-define-key "\C-hv"
			  ;;(concat (char-to-string help-char) "v") ; does not work
			  'cperl-get-help
			  [(control h) v])
	(cperl-define-key "\C-c\C-hf"
			  ;;(concat (char-to-string help-char) "f") ; does not work
			  (key-binding "\C-hf")
			  [(control c) (control h) f])
	(cperl-define-key "\C-c\C-hv"
			  ;;(concat (char-to-string help-char) "v") ; does not work
			  (key-binding "\C-hv")
			  [(control c) (control h) v]))
    (cperl-define-key "\C-c\C-hf" 'cperl-info-on-current-command
		      [(control c) (control h) f])
    (cperl-define-key "\C-c\C-hv"
		      ;;(concat (char-to-string help-char) "v") ; does not work
		      'cperl-get-help
		      [(control c) (control h) v]))
  (if (and cperl-xemacs-p
	   (<= emacs-minor-version 11) (<= emacs-major-version 19))
      (progn
	;; substitute-key-definition is usefulness-deenhanced...
	(cperl-define-key "\M-q" 'cperl-fill-paragraph)
	(cperl-define-key "\e;" 'cperl-indent-for-comment)
	(cperl-define-key "\e\C-\\" 'cperl-indent-region))
    (substitute-key-definition
     'indent-sexp 'cperl-indent-exp
     cperl-mode-map global-map)
    (substitute-key-definition
     'fill-paragraph 'cperl-fill-paragraph
     cperl-mode-map global-map)
    (substitute-key-definition
     'indent-region 'cperl-indent-region
     cperl-mode-map global-map)
    (substitute-key-definition
     'indent-for-comment 'cperl-indent-for-comment
     cperl-mode-map global-map)))

(defvar cperl-menu)
(defvar cperl-lazy-installed)
(defvar cperl-old-style nil)
(condition-case nil
    (progn
      (require 'easymenu)
      (easy-menu-define cperl-menu cperl-mode-map "Menu for CPerl mode"
         '("Perl"
	   ["Beginning of function" beginning-of-defun t]
	   ["End of function" end-of-defun t]
	   ["Mark function" mark-defun t]
	   ["Indent expression" cperl-indent-exp t]
	   ["Fill paragraph/comment" cperl-fill-paragraph t]
	   "----"
	   ["Line up a construction" cperl-lineup (cperl-use-region-p)]
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
	     cperl-use-syntax-table-text-property])
	   ["Refresh \"hard\" constructions" cperl-find-pods-heres t]
	   "----"
	   ["Indent region" cperl-indent-region (cperl-use-region-p)]
	   ["Comment region" cperl-comment-region (cperl-use-region-p)]
	   ["Uncomment region" cperl-uncomment-region (cperl-use-region-p)]
	   "----"
	   ["Run" mode-compile (fboundp 'mode-compile)]
	   ["Kill" mode-compile-kill (and (fboundp 'mode-compile-kill)
					  (get-buffer "*compilation*"))]
	   ["Next error" next-error (get-buffer "*compilation*")]
	   ["Check syntax" cperl-check-syntax (fboundp 'mode-compile)]
	   "----"
	   ["Debugger" cperl-db t]
	   "----"
	   ("Tools"
	    ["Imenu" imenu (fboundp 'imenu)]
	    ["Insert spaces if needed" cperl-find-bad-style t]
	    ["Class Hierarchy from TAGS" cperl-tags-hier-init t]
	    ;;["Update classes" (cperl-tags-hier-init t) tags-table-list]
	    ["CPerl pretty print (exprmntl)" cperl-ps-print
	     (fboundp 'ps-extend-face-list)]
	    ["Imenu on info" cperl-imenu-on-info (featurep 'imenu)]
	    ("Tags"
;;;	     ["Create tags for current file" cperl-etags t]
;;;	     ["Add tags for current file" (cperl-etags t) t]
;;;	     ["Create tags for Perl files in directory" (cperl-etags nil t) t]
;;;	     ["Add tags for Perl files in directory" (cperl-etags t t) t]
;;;	     ["Create tags for Perl files in (sub)directories"
;;;	      (cperl-etags nil 'recursive) t]
;;;	     ["Add tags for Perl files in (sub)directories"
;;;	      (cperl-etags t 'recursive) t])
;;;; cperl-write-tags (&optional file erase recurse dir inbuffer)
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
	     (fboundp 'imenu-go-find-at-position)]
	    ["Help on function" cperl-info-on-command t]
	    ["Help on function at point" cperl-info-on-current-command t]
	    ["Help on symbol at point" cperl-get-help t]
	    ["Perldoc" cperl-perldoc t]
	    ["Perldoc on word at point" cperl-perldoc-at-point t]
	    ["View manpage of POD in this file" cperl-pod-to-manpage t]
	    ["Auto-help on" cperl-lazy-install
	     (and (fboundp 'run-with-idle-timer)
		  (not cperl-lazy-installed))]
	    ["Auto-help off" (eval '(cperl-lazy-unstall))
	     (and (fboundp 'run-with-idle-timer)
		  cperl-lazy-installed)])
	   ("Toggle..."
	    ["Auto newline" cperl-toggle-auto-newline t]
	    ["Electric parens" cperl-toggle-electric t]
	    ["Electric keywords" cperl-toggle-abbrev t]
	    ["Fix whitespace on indent" cperl-toggle-construct-fix t]
	    ["Auto fill" auto-fill-mode t])
	   ("Indent styles..."
	    ["CPerl" (cperl-set-style "CPerl") t]
	    ["PerlStyle" (cperl-set-style "PerlStyle") t]
	    ["GNU" (cperl-set-style "GNU") t]
	    ["C++" (cperl-set-style "C++") t]
	    ["FSF" (cperl-set-style "FSF") t]
	    ["BSD" (cperl-set-style "BSD") t]
	    ["Whitesmith" (cperl-set-style "Whitesmith") t]
	    ["Current" (cperl-set-style "Current") t]
	    ["Memorized" (cperl-set-style-back) cperl-old-style])
	   ("Micro-docs"
	    ["Tips" (describe-variable 'cperl-tips) t]
	    ["Problems" (describe-variable 'cperl-problems) t]
	    ["Non-problems" (describe-variable 'cperl-non-problems) t]
	    ["Speed" (describe-variable 'cperl-speed) t]
	    ["Praise" (describe-variable 'cperl-praise) t]
	    ["Faces" (describe-variable 'cperl-tips-faces) t]
	    ["CPerl mode" (describe-function 'cperl-mode) t]
	    ["CPerl version"
	     (message "The version of master-file for this CPerl is %s-emacs"
		      cperl-version) t]))))
  (error nil))

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)

(defvar cperl-mode-syntax-table nil
  "Syntax table in use in Cperl-mode buffers.")

(defvar cperl-string-syntax-table nil
  "Syntax table in use in Cperl-mode string-like chunks.")

(if cperl-mode-syntax-table
    ()
  (setq cperl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" cperl-mode-syntax-table)
  (modify-syntax-entry ?/ "." cperl-mode-syntax-table)
  (modify-syntax-entry ?* "." cperl-mode-syntax-table)
  (modify-syntax-entry ?+ "." cperl-mode-syntax-table)
  (modify-syntax-entry ?- "." cperl-mode-syntax-table)
  (modify-syntax-entry ?= "." cperl-mode-syntax-table)
  (modify-syntax-entry ?% "." cperl-mode-syntax-table)
  (modify-syntax-entry ?< "." cperl-mode-syntax-table)
  (modify-syntax-entry ?> "." cperl-mode-syntax-table)
  (modify-syntax-entry ?& "." cperl-mode-syntax-table)
  (modify-syntax-entry ?$ "\\" cperl-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cperl-mode-syntax-table)
  (modify-syntax-entry ?# "<" cperl-mode-syntax-table)
  (modify-syntax-entry ?' "\"" cperl-mode-syntax-table)
  (modify-syntax-entry ?` "\"" cperl-mode-syntax-table)
  (if cperl-under-as-char
      (modify-syntax-entry ?_ "w" cperl-mode-syntax-table))
  (modify-syntax-entry ?: "_" cperl-mode-syntax-table)
  (modify-syntax-entry ?| "." cperl-mode-syntax-table)
  (setq cperl-string-syntax-table (copy-syntax-table cperl-mode-syntax-table))
  (modify-syntax-entry ?$ "." cperl-string-syntax-table)
  (modify-syntax-entry ?# "." cperl-string-syntax-table) ; (?# comment )
)



(defvar cperl-faces-init nil)
;; Fix for msb.el
(defvar cperl-msb-fixed nil)
;;;###autoload
(defun cperl-mode ()
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
since most the time you mean \"less\".  Cperl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate.  You can set `cperl-electric-parens-string' to the string that
contains the parenths from the above list you want to be electrical.
Electricity of parenths is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:

   if, else, elsif, unless, while, until, continue, do,
   for, foreach, formy and foreachmy.

and POD directives (Disabled by default, see `cperl-electric-keywords'.)

The user types the keyword immediately followed by a space, which
causes the construct to be expanded, and the point is positioned where
she is most likely to want to be.  eg. when the user types a space
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
`newline-and-indent' behaviour, it is on \\[newline-and-indent],
see documentation on `cperl-electric-linefeed'.

Use \\[cperl-invert-if-unless] to change a construction of the form

	    if (A) { B }

into

            B if A;

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on font-lock-mode
\(even with older Emacsen), `cperl-electric-lbrace-space' to t switches
on electric space between $ and {, `cperl-electric-parens-string' is
the string that contains parentheses that should be electric in CPerl
\(see also `cperl-electric-parens-mark' and `cperl-electric-parens'),
setting `cperl-electric-keywords' enables electric expansion of
control structures in CPerl.  `cperl-electric-linefeed' governs which
one of two linefeed behavior is preferable.  You can enable all these
options simultaneously (recommended mode of use) by setting
`cperl-hairy' to t.  In this case you can switch separate options off
by setting them to `null'.  Note that one may undo the extra
whitespace inserted by semis and braces in `auto-newline'-mode by
consequent \\[cperl-electric-backspace].

If your site has perl5 documentation in info format, you can use commands
\\[cperl-info-on-current-command] and \\[cperl-info-on-command] to access it.
These keys run commands `cperl-info-on-current-command' and
`cperl-info-on-command', which one is which is controlled by variable
`cperl-info-on-command-no-prompt' and `cperl-clobber-lisp-bindings'
\(in turn affected by `cperl-hairy').

Even if you have no info-format documentation, short one-liner-style
help is available on \\[cperl-get-help], and one can run perldoc or
man via menu.

It is possible to show this help automatically after some idle time.
This is regulated by variable `cperl-lazy-help-time'.  Default with
`cperl-hairy' (if the value of `cperl-lazy-help-time' is nil) is 5
secs idle time .  It is also possible to switch this on/off from the
menu, or via \\[cperl-toggle-autohelp].  Requires `run-with-idle-timer'.

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of pod and
here-docs sections.  With capable Emaxen results of scan are used
for indentation too, otherwise they are used for highlighting only.

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

Settings for K&R and BSD indentation styles are
  `cperl-indent-level'                5    8
  `cperl-continued-statement-offset'  5    8
  `cperl-brace-offset'               -5   -8
  `cperl-label-offset'               -5   -8

CPerl knows several indentation styles, and may bulk set the
corresponding variables.  Use \\[cperl-set-style] to do this.  Use
\\[cperl-set-style-back] to restore the memorized preexisting values
\(both available from menu).

If `cperl-indent-level' is 0, the statement after opening brace in
column 0 is indented on
`cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args.

DO NOT FORGET to read micro-docs (available from `Perl' menu)
or as help on variables `cperl-tips', `cperl-problems',
`cperl-non-problems', `cperl-praise', `cperl-speed'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map cperl-mode-map)
  (if (cperl-val 'cperl-electric-linefeed)
      (progn
	(local-set-key "\C-J" 'cperl-linefeed)
	(local-set-key "\C-C\C-J" 'newline-and-indent)))
  (if (and
       (cperl-val 'cperl-clobber-lisp-bindings)
       (cperl-val 'cperl-info-on-command-no-prompt))
      (progn
	;; don't clobber the backspace binding:
	(cperl-define-key "\C-hf" 'cperl-info-on-current-command [(control h) f])
	(cperl-define-key "\C-c\C-hf" 'cperl-info-on-command
			  [(control c) (control h) f])))
  (setq major-mode 'cperl-mode)
  (setq mode-name "CPerl")
  (if (not cperl-mode-abbrev-table)
      (let ((prev-a-c abbrevs-changed))
	(define-abbrev-table 'cperl-mode-abbrev-table '(
		("if" "if" cperl-electric-keyword 0)
		("elsif" "elsif" cperl-electric-keyword 0)
		("while" "while" cperl-electric-keyword 0)
		("until" "until" cperl-electric-keyword 0)
		("unless" "unless" cperl-electric-keyword 0)
		("else" "else" cperl-electric-else 0)
		("continue" "continue" cperl-electric-else 0)
		("for" "for" cperl-electric-keyword 0)
		("foreach" "foreach" cperl-electric-keyword 0)
		("formy" "formy" cperl-electric-keyword 0)
		("foreachmy" "foreachmy" cperl-electric-keyword 0)
		("do" "do" cperl-electric-keyword 0)
		("=pod" "=pod" cperl-electric-pod 0)
		("=over" "=over" cperl-electric-pod 0)
		("=head1" "=head1" cperl-electric-pod 0)
		("=head2" "=head2" cperl-electric-pod 0)
		("pod" "pod" cperl-electric-pod 0)
		("over" "over" cperl-electric-pod 0)
		("head1" "head1" cperl-electric-pod 0)
		("head2" "head2" cperl-electric-pod 0)))
	(setq abbrevs-changed prev-a-c)))
  (setq local-abbrev-table cperl-mode-abbrev-table)
  (abbrev-mode (if (cperl-val 'cperl-electric-keywords) 1 0))
  (set-syntax-table cperl-mode-syntax-table)
  (make-local-variable 'outline-regexp)
  ;; (setq outline-regexp imenu-example--function-name-regexp-perl)
  (setq outline-regexp cperl-outline-regexp)
  (make-local-variable 'outline-level)
  (setq outline-level 'cperl-outline-level)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cperl-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column cperl-comment-column)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'defun-prompt-regexp)
  (setq defun-prompt-regexp "^[ \t]*sub[ \t]+\\([^ \t\n{(;]+\\)[ \t]*")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'cperl-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'cperl-indent-region)
  ;;(setq auto-fill-function 'cperl-do-auto-fill) ; Need to switch on and off!
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function
	(function cperl-imenu--create-perl-index))
  (make-local-variable 'imenu-sort-function)
  (setq imenu-sort-function nil)
  (make-local-variable 'vc-header-alist)
  (set 'vc-header-alist cperl-vc-header-alist) ; Avoid warning
  (make-local-variable 'font-lock-defaults)
  (setq	font-lock-defaults
	(cond
	 ((string< emacs-version "19.30")
	  '(cperl-font-lock-keywords-2))
	 ((string< emacs-version "19.33") ; Which one to use?
	  '((cperl-font-lock-keywords
	     cperl-font-lock-keywords-1
	     cperl-font-lock-keywords-2)))
	 (t
	  '((cperl-load-font-lock-keywords
	     cperl-load-font-lock-keywords-1
	     cperl-load-font-lock-keywords-2)))))
  (make-local-variable 'cperl-syntax-state)
  (if cperl-use-syntax-table-text-property
      (progn
	(make-local-variable 'parse-sexp-lookup-properties)
	;; Do not introduce variable if not needed, we check it!
	(set 'parse-sexp-lookup-properties t)
	;; Fix broken font-lock:
	(or (boundp 'font-lock-unfontify-region-function)
	    (set 'font-lock-unfontify-region-function
		  'font-lock-default-unfontify-region))
	(make-local-variable 'font-lock-unfontify-region-function)
	(set 'font-lock-unfontify-region-function
	      'cperl-font-lock-unfontify-region-function)
	(make-local-variable 'cperl-syntax-done-to)
	;; Another bug: unless font-lock-syntactic-keywords, font-lock
	;;  ignores syntax-table text-property.  (t) is a hack
	;;  to make font-lock think that font-lock-syntactic-keywords
	;;  are defined
	(make-local-variable 'font-lock-syntactic-keywords)
	(setq font-lock-syntactic-keywords
	      (if cperl-syntaxify-by-font-lock
		  '(t (cperl-fontify-syntaxically))
		'(t)))))
  (make-local-variable 'cperl-old-style)
  (set (make-local-variable 'normal-auto-fill-function)
       #'cperl-do-auto-fill)
  (if (cperl-enable-font-lock)
      (if (cperl-val 'cperl-font-lock)
	  (progn (or cperl-faces-init (cperl-init-faces))
		 (font-lock-mode 1))))
  (and (boundp 'msb-menu-cond)
       (not cperl-msb-fixed)
       (cperl-msb-fix))
  (if (featurep 'easymenu)
      (easy-menu-add cperl-menu))	; A NOP in Emacs.
  (run-hooks 'cperl-mode-hook)
  ;; After hooks since fontification will break this
  (if cperl-pod-here-scan
      (or ;;(and (boundp 'font-lock-mode)
	  ;;     (eval 'font-lock-mode)	; Avoid warning
	  ;;     (boundp 'font-lock-hot-pass) ; Newer font-lock
       cperl-syntaxify-by-font-lock  ;;)
       (progn (or cperl-faces-init (cperl-init-faces-weak))
	      (cperl-find-pods-heres)))))

;; Fix for perldb - make default reasonable
(defun cperl-db ()
  (interactive)
  (require 'gud)
  (perldb (read-from-minibuffer "Run perldb (like this): "
				(if (consp gud-perldb-history)
				    (car gud-perldb-history)
				  (concat "perl " ;;(file-name-nondirectory
						   ;; I have problems
						   ;; in OS/2
						   ;; otherwise
						   (buffer-file-name)))
				nil nil
				'(gud-perldb-history . 1))))

(defun cperl-msb-fix ()
  ;; Adds perl files to msb menu, supposes that msb is already loaded
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
  (let ((p (point)) (c (current-column)) was phony)
    (if (looking-at "^#") 0		; Existing comment at bol stays there.
      ;; Wrong comment found
      (save-excursion
	(setq was (cperl-to-comment-or-eol)
	      phony (eq (get-text-property (point) 'syntax-table)
			cperl-st-cfence))
	(if phony
	    (progn
	      (re-search-forward "#\\|$") ; Hmm, what about embedded #?
	      (if (eq (preceding-char) ?\#)
		  (forward-char -1))
	      (setq was nil)))
	(if (= (point) p)
	    (progn
	      (skip-chars-backward " \t")
	      (max (1+ (current-column)) ; Else indent at comment column
		   comment-column))
	  (if was nil
	    (insert comment-start)
	    (backward-char (length comment-start)))
	  (setq cperl-wrong-comment t)
	  (indent-to comment-column 1)	; Indent minimum 1
	  c)))))			; except leave at least one space.

;;;(defun cperl-comment-indent-fallback ()
;;;  "Is called if the standard comment-search procedure fails.
;;;Point is at start of real comment."
;;;  (let ((c (current-column)) target cnt prevc)
;;;    (if (= c comment-column) nil
;;;      (setq cnt (skip-chars-backward "[ \t]"))
;;;      (setq target (max (1+ (setq prevc
;;;			     (current-column))) ; Else indent at comment column
;;;		   comment-column))
;;;      (if (= c comment-column) nil
;;;	(delete-backward-char cnt)
;;;	(while (< prevc target)
;;;	  (insert "\t")
;;;	  (setq prevc (current-column)))
;;;	(if (> prevc target) (progn (delete-char -1) (setq prevc (current-column))))
;;;	(while (< prevc target)
;;;	  (insert " ")
;;;	  (setq prevc (current-column)))))))

(defun cperl-indent-for-comment ()
  "Substitute for `indent-for-comment' in CPerl."
  (interactive)
  (let (cperl-wrong-comment)
    (indent-for-comment)
    (if cperl-wrong-comment
	(progn (cperl-to-comment-or-eol)
	       (forward-char (length comment-start))))))

(defun cperl-comment-region (b e arg)
  "Comment or uncomment each line in the region in CPerl mode.
See `comment-region'."
  (interactive "r\np")
  (let ((comment-start "#"))
    (comment-region b e arg)))

(defun cperl-uncomment-region (b e arg)
  "Uncomment or comment each line in the region in CPerl mode.
See `comment-region'."
  (interactive "r\np")
  (let ((comment-start "#"))
    (comment-region b e (- arg))))

(defvar cperl-brace-recursing nil)

(defun cperl-electric-brace (arg &optional only-before)
  "Insert character and correct line's indentation.
If ONLY-BEFORE and `cperl-auto-newline', will insert newline before the
place (even in empty line), but not after.  If after \")\" and the inserted
char is \"{\", insert extra newline before only if
`cperl-extra-newline-before-brace'."
  (interactive "P")
  (let (insertpos
	(other-end (if (and cperl-electric-parens-mark
			    (cperl-mark-active)
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
	    (setq last-command-char ?\{)
	    (cperl-electric-lbrace arg insertpos))
	  (forward-char 1))
      ;: Check whether we close something "usual" with `}'
      (if (and (eq last-command-char ?\})
	       (not
		(condition-case nil
		    (save-excursion
		      (up-list (- (prefix-numeric-value arg)))
		      ;;(cperl-after-block-p (point-min))
		      (cperl-after-expr-p nil "{;)"))
		  (error nil))))
	  ;; Just insert the guy
	  (self-insert-command (prefix-numeric-value arg))
	(if (and (not arg)		; No args, end (of empty line or auto)
		 (eolp)
		 (or (and (null only-before)
			  (save-excursion
			    (skip-chars-backward " \t")
			    (bolp)))
		     (and (eq last-command-char ?\{) ; Do not insert newline
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
						      1 last-command-char))
				     (setq insertpos (1- (point)))))
		(delete-char -1))))
	(if insertpos
	    (save-excursion
	      (goto-char insertpos)
	      (self-insert-command (prefix-numeric-value arg)))
	  (self-insert-command (prefix-numeric-value arg)))))))

(defun cperl-electric-lbrace (arg &optional end)
  "Insert character, correct line's indentation, correct quoting by space."
  (interactive "P")
  (let (pos after
	    (cperl-brace-recursing t)
	    (cperl-auto-newline cperl-auto-newline)
	    (other-end (or end
			   (if (and cperl-electric-parens-mark
				    (cperl-mark-active)
				    (> (mark) (point)))
			       (save-excursion
				 (goto-char (mark))
				 (point-marker))
			     nil))))
    (and (cperl-val 'cperl-electric-lbrace-space)
	 (eq (preceding-char) ?$)
	 (save-excursion
	   (skip-chars-backward "$")
	   (looking-at "\\(\\$\\$\\)*\\$\\([^\\$]\\|$\\)"))
	 (insert ?\ ))
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
	 (eq last-command-char ?{)
	 (memq last-command-char
	       (append cperl-electric-parens-string nil))
	 (or (if other-end (goto-char (marker-position other-end)))
	     t)
	 (setq last-command-char ?} pos (point))
	 (progn (cperl-electric-brace arg t)
		(goto-char pos)))))

(defun cperl-electric-paren (arg)
  "Insert a matching pair of parentheses."
  (interactive "P")
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(other-end (if (and cperl-electric-parens-mark
			    (cperl-mark-active)
			    (> (mark) (point)))
			   (save-excursion
			     (goto-char (mark))
			     (point-marker))
		     nil)))
    (if (and (cperl-val 'cperl-electric-parens)
	     (memq last-command-char
		   (append cperl-electric-parens-string nil))
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point))
	     ;;(not (save-excursion (search-backward "#" beg t)))
	     (if (eq last-command-char ?<)
		 (progn
		   (and abbrev-mode ; later it is too late, may be after `for'
			(expand-abbrev))
		   (cperl-after-expr-p nil "{;(,:="))
	       1))
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  (if other-end (goto-char (marker-position other-end)))
	  (insert (make-string
		   (prefix-numeric-value arg)
		   (cdr (assoc last-command-char '((?{ .?})
						   (?[ . ?])
						   (?( . ?))
						   (?< . ?>))))))
	  (forward-char (- (prefix-numeric-value arg))))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-rparen (arg)
  "Insert a matching pair of parentheses if marking is active.
If not, or if we are not at the end of marking range, would self-insert."
  (interactive "P")
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(other-end (if (and cperl-electric-parens-mark
			    (cperl-val 'cperl-electric-parens)
			    (memq last-command-char
				  (append cperl-electric-parens-string nil))
			    (cperl-mark-active)
			    (< (mark) (point)))
		       (mark)
		     nil))
	p)
    (if (and other-end
	     (cperl-val 'cperl-electric-parens)
	     (memq last-command-char '( ?\) ?\] ?\} ?\> ))
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point))
	     ;;(not (save-excursion (search-backward "#" beg t)))
	     )
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  (setq p (point))
	  (if other-end (goto-char other-end))
	  (insert (make-string
		   (prefix-numeric-value arg)
		   (cdr (assoc last-command-char '((?\} . ?\{)
						  (?\] . ?\[)
						  (?\) . ?\()
						  (?\> . ?\<))))))
	  (goto-char (1+ p)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-keyword ()
  "Insert a construction appropriate after a keyword.
Help message may be switched off by setting `cperl-message-electric-keyword'
to nil."
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(dollar (and (eq last-command-char ?$)
		     (eq this-command 'self-insert-command)))
	(delete (and (memq last-command-char '(?\ ?\n ?\t ?\f))
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
			      (and cperl-use-syntax-table-text-property
				   (not (eq (get-text-property (point)
							       'syntax-type)
					    'pod))))))
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
 	     (insert (if do " {\n} while ();" " () {\n}")))
 	    )
	   (or (looking-at "[ \t]\\|$") (insert " "))
	   (cperl-indent-line)
	   (if dollar (progn (search-backward "$")
			     (if my
				 (forward-char 1)
			       (delete-char 1)))
	     (search-backward ")"))
	   (if delete
	       (cperl-putback-char cperl-del-back-ch))
	   (if cperl-message-electric-keyword
	       (message "Precede char by C-q to avoid expansion"))))))

(defun cperl-ensure-newlines (n &optional pos)
  "Make sure there are N newlines after the point."
  (or pos (setq pos (point)))
  (if (looking-at "\n")
      (forward-char 1)
    (insert "\n"))
  (if (> n 1)
      (cperl-ensure-newlines (1- n) pos)
    (goto-char pos)))

(defun cperl-electric-pod ()
  "Insert a POD chunk appropriate after a =POD directive."
  (let ((delete (and (memq last-command-char '(?\ ?\n ?\t ?\f))
		     (memq this-command '(self-insert-command newline))))
	head1 notlast name p really-delete over)
    (and (save-excursion
	   (forward-word -1)
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
	     (and (re-search-backward
		   ;; "\\(\\`\n?\\|\n\n\\)=\\sw+" 
		   "\\(\\`\n?\\|^\n\\)=\\sw+" 
		   (point-min) t)
		  (not (or
			(looking-at "=cut")
			(and cperl-use-syntax-table-text-property
			     (not (eq (get-text-property (point) 'syntax-type)
				      'pod)))))))))
	 (progn
	   (save-excursion
	     (setq notlast (re-search-forward "^\n=" nil t)))
	   (or notlast
	       (progn
		 (insert "\n\n=cut")
		 (cperl-ensure-newlines 2)
		 (forward-word -2)
		 (if (and head1 
			  (not 
			   (save-excursion
			     (forward-char -1)
			     (re-search-backward "\\(\\`\n?\\|\n\n\\)=head1\\>"
						nil t)))) ; Only one
		     (progn 
		       (forward-word 1)
		       (setq name (file-name-sans-extension
				   (file-name-nondirectory (buffer-file-name)))
			     p (point))
		       (insert " NAME\n\n" name
			       " - \n\n=head1 SYNOPSIS\n\n\n\n"
			       "=head1 DESCRIPTION")
		       (cperl-ensure-newlines 4)
		       (goto-char p)
		       (forward-word 2)
		       (end-of-line)
		       (setq really-delete t))
		   (forward-word 1))))
	   (if over
	       (progn
		 (setq p (point))
		 (insert "\n\n=item \n\n\n\n"
			 "=back")
		 (cperl-ensure-newlines 2)
		 (goto-char p)
		 (forward-word 1)
		 (end-of-line)
		 (setq really-delete t)))
	   (if (and delete really-delete)
	       (cperl-putback-char cperl-del-back-ch))))))

(defun cperl-electric-else ()
  "Insert a construction appropriate after a keyword.
Help message may be switched off by setting `cperl-message-electric-keyword'
to nil."
  (let ((beg (save-excursion (beginning-of-line) (point))))
    (and (save-excursion
	   (backward-sexp 1)
	   (cperl-after-expr-p nil "{;:"))
	 (save-excursion
	   (not
	    (re-search-backward
	     "[#\"'`]\\|\\<q\\(\\|[wqxr]\\)\\>"
	     beg t)))
	 (save-excursion (or (not (re-search-backward "^=" nil t))
			     (looking-at "=cut")
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
 	     (insert " {\n\n}"))
 	    )
	   (or (looking-at "[ \t]\\|$") (insert " "))
	   (cperl-indent-line)
	   (forward-line -1)
	   (cperl-indent-line)
	   (cperl-putback-char cperl-del-back-ch)
	   (setq this-command 'cperl-electric-else)
	   (if cperl-message-electric-keyword
	       (message "Precede char by C-q to avoid expansion"))))))

(defun cperl-linefeed ()
  "Go to end of line, open a new line and indent appropriately.
If in POD, insert appropriate lines."
  (interactive)
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(end (save-excursion (end-of-line) (point)))
	(pos (point)) start over cut res)
    (if (and				; Check if we need to split:
					; i.e., on a boundary and inside "{...}"
	 (save-excursion (cperl-to-comment-or-eol)
	   (>= (point) pos))		; Not in a comment
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
		(setq cut (looking-at "\\(\\`\n?\\|\n\\)=cut\\>"))
		(setq over (looking-at "\\(\\`\n?\\|\n\\)=over\\>"))
		t)))
	(if (and over
		 (progn
		   (forward-paragraph -1)
		   (forward-word 1)
		   (setq pos (point))
		   (setq cut (buffer-substring (point)
					       (save-excursion
						 (end-of-line)
						 (point))))
		   (delete-char (- (save-excursion (end-of-line) (point))
				   (point)))
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
  "Insert character and correct line's indentation."
  (interactive "P")
  (if cperl-auto-newline
      (cperl-electric-terminator arg)
    (self-insert-command (prefix-numeric-value arg))
    (if cperl-autoindent-on-semi
	(cperl-indent-line))))

(defun cperl-electric-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point))
		  (auto (and cperl-auto-newline
			     (or (not (eq last-command-char ?:))
				 cperl-auto-newline-after-colon))))
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
		     (and (eq last-command-char ?:)
			  (save-excursion
			    (forward-word 1)
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
  "Backspace-untabify, or remove the whitespace around the point inserted
by an electric key."
  (interactive "p")
  (if (and cperl-auto-newline
	   (memq last-command '(cperl-electric-semi
				cperl-electric-terminator
				cperl-electric-lbrace))
	   (memq (preceding-char) '(?\  ?\t ?\n)))
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
	     (memq (preceding-char) '(?\  ?\t ?\n)))
	(let (p)
	  (skip-chars-forward " \t\n")
	  (setq p (point))
	  (skip-chars-backward " \t\n")
	  (delete-region (point) p))
      (backward-delete-char-untabify arg))))

(defun cperl-inside-parens-p ()
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
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (cperl-update-syntaxification (point) (point))
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
Return the amount the indentation changed by."
  (let (indent i beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (setq indent (cperl-calculate-indent parse-data)
	  i indent)
    (beginning-of-line)
    (setq beg (point))
    (cond ((or (eq indent nil) (eq indent t))
	   (setq indent (current-indentation) i nil))
	  ;;((eq indent t)    ; Never?
	  ;; (setq indent (cperl-calculate-indent-within-comment)))
	  ;;((looking-at "[ \t]*#")
	  ;; (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((looking-at "[A-Za-z_][A-Za-z_0-9]*:[^:]")
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
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun cperl-after-label ()
  ;; Returns true if the point is after label.  Does not do save-excursion.
  (and (eq (preceding-char) ?:)
       (memq (char-syntax (char-after (- (point) 2)))
	     '(?w ?_))
       (progn
	 (backward-sexp)
	 (looking-at "[a-zA-Z_][a-zA-Z0-9_]*:[^:]"))))

(defun cperl-get-state (&optional parse-start start-state)
  ;; returns list (START STATE DEPTH PRESTART),
  ;; START is a good place to start parsing, or equal to
  ;; PARSE-START if preset,
  ;; STATE is what is returned by `parse-partial-sexp'.
  ;; DEPTH is true is we are immediately after end of block
  ;; which contains START.
  ;; PRESTART is the position basing on which START was found.
  (save-excursion
    (let ((start-point (point)) depth state start prestart)
      (if (and parse-start
	       (<= parse-start start-point))
	  (goto-char parse-start)
	(beginning-of-defun)
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

(defun cperl-block-p ()			; Do not C-M-q !  One string contains ";" !
  ;; Positions is before ?\{.  Checks whether it starts a block.
  ;; No save-excursion!
  (cperl-backward-to-noncomment (point-min))
  (or (memq (preceding-char) (append ";){}$@&%\C-@" nil)) ; Or label!  \C-@ at bobp
					; Label may be mixed up with `$blah :'
      (save-excursion (cperl-after-label))
      (and (memq (char-syntax (preceding-char)) '(?w ?_))
	   (progn
	     (backward-sexp)
	     ;; Need take into account `bless', `return', `tr',...
	     (or (and (looking-at "[a-zA-Z0-9_:]+[ \t\n\f]*[{#]") ; Method call syntax
		      (not (looking-at "\\(bless\\|return\\|q[wqrx]?\\|tr\\|[smy]\\)\\>")))
		 (progn
		   (skip-chars-backward " \t\n\f")
		   (and (memq (char-syntax (preceding-char)) '(?w ?_))
			(progn
			  (backward-sexp)
			  (looking-at
			   "sub[ \t]+[a-zA-Z0-9_:]+[ \t\n\f]*\\(([^()]*)[ \t\n\f]*\\)?[#{]")))))))))

(defvar cperl-look-for-prop '((pod in-pod) (here-doc-delim here-doc-group)))

(defun cperl-calculate-indent (&optional parse-data) ; was parse-start
  "Return appropriate indentation for current line as Perl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.

Will not correct the indentation for labels, but will correct it for braces
and closing parentheses and brackets.."
  (save-excursion
    (if (or
	 (and (memq (get-text-property (point) 'syntax-type)
		    '(pod here-doc here-doc-delim format))
	      (not (get-text-property (point) 'indentable)))
	 ;; before start of POD - whitespace found since do not have 'pod!
	 (and (looking-at "[ \t]*\n=")
	      (error "Spaces before pod section!"))
	 (and (not cperl-indent-left-aligned-comments)
	      (looking-at "^#")))
	nil
     (beginning-of-line)
     (let ((indent-point (point))
	   (char-after (save-excursion
			   (skip-chars-forward " \t")
			   (following-char)))
	   (in-pod (get-text-property (point) 'in-pod))
	   (pre-indent-point (point))
	   p prop look-prop is-block delim)
      (cond
       (in-pod
	;; In the verbatim part, probably code example.  What to do???
	)
       (t
	(save-excursion
	  ;; Not in pod
	  (cperl-backward-to-noncomment nil)
	  (setq p (max (point-min) (1- (point)))
		prop (get-text-property p 'syntax-type)
		look-prop (or (nth 1 (assoc prop cperl-look-for-prop))
			      'syntax-type))
	  (if (memq prop '(pod here-doc format here-doc-delim))
	      (progn
		(goto-char (or (previous-single-property-change p look-prop)
			       (point-min)))
		(beginning-of-line)
		(setq pre-indent-point (point)))))))
      (goto-char pre-indent-point)
      (let* ((case-fold-search nil)
	     (s-s (cperl-get-state (car parse-data) (nth 1 parse-data)))
	     (start (or (nth 2 parse-data)
			(nth 0 s-s)))
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
	       ;; indent to just after the surrounding open,
	       ;; skip blanks if we do not close the expression.
	       (goto-char (1+ (previous-single-property-change (point) 'indentable)))
	       (or (memq char-after (append ")]}" nil))
		   (looking-at "[ \t]*\\(#\\|$\\)")
		   (skip-chars-forward " \t"))
	       (current-column))
	      ((or (nth 3 state) (nth 4 state))
	       ;; return nil or t if should not change this line
	       (nth 4 state))
	      ;; XXXX Do we need to special-case this?
	      ((null containing-sexp)
	       ;; Line is at top level.  May be data or function definition,
	       ;; or may be function argument declaration.
	       ;; Indent like the previous top level line
	       ;; unless that ends in a closeparen without semicolon,
	       ;; in which case this line is the first argument decl.
	       (skip-chars-forward " \t")
	       (+ (save-excursion
		    (goto-char start)
		    (- (current-indentation)
		       (if (nth 2 s-s) cperl-indent-level 0)))
		  (if (= char-after ?{) cperl-continued-brace-offset 0)
		  (progn
		    (cperl-backward-to-noncomment (or old-indent (point-min)))
		    ;; Look at previous line that's at column 0
		    ;; to determine whether we are in top-level decls
		    ;; or function's arg decls.  Set basic-indent accordingly.
		    ;; Now add a little if this is a continuation line.
		    (if (or (bobp)
			    (eq (point) old-indent) ; old-indent was at comment
			    (eq (preceding-char) ?\;)
			    ;;  Had ?\) too
			    (and (eq (preceding-char) ?\})
				 (cperl-after-block-and-statement-beg
				  (point-min)))	; Was start - too close
			    (memq char-after (append ")]}" nil))
			    (and (eq (preceding-char) ?\:) ; label
				 (progn
				   (forward-sexp -1)
				   (skip-chars-backward " \t")
				   (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*[ \t]*:"))))
			(progn
			  (if (and parse-data
				   (not (eq char-after ?\C-j)))
			      (setcdr (cddr parse-data)
				      (list pre-indent-point)))
			  0)
		      cperl-continued-statement-offset))))
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
	       (or (memq char-after (append ")]}" nil))
		   (looking-at "[ \t]*\\(#\\|$\\)")
		   (skip-chars-forward " \t"))
	       (current-column))
	      ((progn
		 ;; Containing-expr starts with \{.  Check whether it is a hash.
		 (goto-char containing-sexp)
		 (not (cperl-block-p)))
	       (goto-char (1+ containing-sexp))
	       (or (memq char-after
			 (append (if delim "}" ")]}") nil))
		   (looking-at "[ \t]*\\(#\\|$\\)")
		   (skip-chars-forward " \t"))
	       (+ (current-column)
		  (if (and delim
			   (eq char-after ?\}))
		      ;; Correct indentation of trailing ?\}
		      (+ cperl-indent-level cperl-close-paren-offset)
		    0)))
;;;	      ((and (/= (char-after containing-sexp) ?{)
;;;		    (not cperl-indent-parens-as-block))
;;;	       ;; line is expression, not statement:
;;;	       ;; indent to just after the surrounding open,
;;;	       ;; skip blanks if we do not close the expression.
;;;	       (goto-char (1+ containing-sexp))
;;;	       (or (memq char-after (append ")]}" nil))
;;;		   (looking-at "[ \t]*\\(#\\|$\\)")
;;;		   (skip-chars-forward " \t"))
;;;	       (current-column))
;;;	      ((progn
;;;		 ;; Containing-expr starts with \{.  Check whether it is a hash.
;;;		 (goto-char containing-sexp)
;;;		 (and (not (cperl-block-p))
;;;		      (not cperl-indent-parens-as-block)))
;;;	       (goto-char (1+ containing-sexp))
;;;	       (or (eq char-after ?\})
;;;		   (looking-at "[ \t]*\\(#\\|$\\)")
;;;		   (skip-chars-forward " \t"))
;;;	       (+ (current-column)	; Correct indentation of trailing ?\}
;;;		  (if (eq char-after ?\}) (+ cperl-indent-level
;;;					     cperl-close-paren-offset) 
;;;		    0)))
	      (t
	       ;; Statement level.  Is it a continuation or a new statement?
	       ;; Find previous non-comment character.
	       (goto-char pre-indent-point)
	       (cperl-backward-to-noncomment containing-sexp)
	       ;; Back up over label lines, since they don't
	       ;; affect whether our line is a continuation.
	       ;; (Had \, too)
	       (while ;;(or (eq (preceding-char) ?\,)
			  (and (eq (preceding-char) ?:)
			       (or;;(eq (char-after (- (point) 2)) ?\') ; ????
				(memq (char-syntax (char-after (- (point) 2)))
				      '(?w ?_))))
			  ;;)
		 (if (eq (preceding-char) ?\,)
		     ;; Will go to beginning of line, essentially.
		     ;; Will ignore embedded sexpr XXXX.
		     (cperl-backward-to-start-of-continued-exp containing-sexp))
		 (beginning-of-line)
		 (cperl-backward-to-noncomment containing-sexp))
	       ;; Now we get the answer.
	       (if (not (or (eq (1- (point)) containing-sexp)
			    (memq (preceding-char)
				  (append (if is-block " ;{" " ,;{") '(nil)))
			    (and (eq (preceding-char) ?\})
				 (cperl-after-block-and-statement-beg 
				  containing-sexp))))
		   ;; This line is continuation of preceding line's statement;
		   ;; indent  `cperl-continued-statement-offset'  more than the
		   ;; previous line of the statement.
		   ;;
		   ;; There might be a label on this line, just
		   ;; consider it bad style and ignore it.
		   (progn
		     (cperl-backward-to-start-of-continued-exp containing-sexp)
		     (+ (if (memq char-after (append "}])" nil))
			    0		; Closing parenth
			  cperl-continued-statement-offset)
			(if (or is-block 
				(not delim)
				(not (eq char-after ?\})))
			    0
			  ;; Now it is a hash reference
			  (+ cperl-indent-level cperl-close-paren-offset))
			(if (looking-at "\\w+[ \t]*:")
			    (if (> (current-indentation) cperl-min-label-indent)
				(- (current-indentation) cperl-label-offset)
			      ;; Do not move `parse-data', this should
			      ;; be quick anyway (this comment comes
			      ;;from different location):
			      (cperl-calculate-indent))
			  (current-column))
			(if (eq char-after ?\{)
			    cperl-continued-brace-offset 0)))
		 ;; This line starts a new statement.
		 ;; Position following last unclosed open.
		 (goto-char containing-sexp)
		 ;; Is line first statement after an open-brace?
		 (or
		  ;; If no, find that first statement and indent like
		  ;; it.  If the first statement begins with label, do
		  ;; not believe when the indentation of the label is too
		  ;; small.
		  (save-excursion
		    (forward-char 1)
		    (setq old-indent (current-indentation))
		    (let ((colon-line-end 0))
		      (while (progn (skip-chars-forward " \t\n")
				    (looking-at "#\\|[a-zA-Z0-9_$]*:[^:]"))
			;; Skip over comments and labels following openbrace.
			(cond ((= (following-char) ?\#)
			       (forward-line 1))
			      ;; label:
			      (t
			       (save-excursion (end-of-line)
					       (setq colon-line-end (point)))
			       (search-forward ":"))))
		      ;; The first following code counts
		      ;; if it is before the line we want to indent.
		      (and (< (point) indent-point)
			   (if (> colon-line-end (point)) ; After label
			       (if (> (current-indentation)
				      cperl-min-label-indent)
				   (- (current-indentation) cperl-label-offset)
				 ;; Do not believe: `max' is involved
				 (+ old-indent cperl-indent-level))
			     (current-column)))))
		  ;; If no previous statement,
		  ;; indent it relative to line brace is on.
		  ;; For open brace in column zero, don't let statement
		  ;; start there too.  If cperl-indent-level is zero,
		  ;; use cperl-brace-offset + cperl-continued-statement-offset instead.
		  ;; For open-braces not the first thing in a line,
		  ;; add in cperl-brace-imaginary-offset.

		  ;; If first thing on a line:  ?????
		  (+ (if (and (bolp) (zerop cperl-indent-level))
			 (+ cperl-brace-offset cperl-continued-statement-offset)
		       cperl-indent-level)
		     (if (or is-block 
			     (not delim)
			     (not (eq char-after ?\})))
			 0
		       ;; Now it is a hash reference
		       (+ cperl-indent-level cperl-close-paren-offset))
		     ;; Move back over whitespace before the openbrace.
		     ;; If openbrace is not first nonwhite thing on the line,
		     ;; add the cperl-brace-imaginary-offset.
		     (progn (skip-chars-backward " \t")
			    (if (bolp) 0 cperl-brace-imaginary-offset))
		     ;; If the openbrace is preceded by a parenthesized exp,
		     ;; move to the beginning of that;
		     ;; possibly a different line
		     (progn
		       (if (eq (preceding-char) ?\))
			   (forward-sexp -1))
		       ;; In the case it starts a subroutine, indent with
		       ;; respect to `sub', not with respect to the
		       ;; first thing on the line, say in the case of
		       ;; anonymous sub in a hash.
		       ;;
		       (skip-chars-backward " \t")
		       (if (and (eq (preceding-char) ?b)
				(progn
				  (forward-sexp -1)
				  (looking-at "sub\\>"))
				(setq old-indent
				      (nth 1
					   (parse-partial-sexp
					    (save-excursion (beginning-of-line) (point))
					    (point)))))
			   (progn (goto-char (1+ old-indent))
				  (skip-chars-forward " \t")
				  (current-column))
			 ;; Get initial indentation of the line we are on.
			 ;; If line starts with label, calculate label indentation
			 (if (save-excursion
			       (beginning-of-line)
			       (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*:[^:]"))
			     (if (> (current-indentation) cperl-min-label-indent)
				 (- (current-indentation) cperl-label-offset)
			       ;; Do not move `parse-data', this should
			       ;; be quick anyway:
			       (cperl-calculate-indent))
			   (current-indentation))))))))))))))

(defvar cperl-indent-alist
  '((string nil)
    (comment nil)
    (toplevel 0)
    (toplevel-after-parenth 2)
    (toplevel-continued 2)
    (expression 1))
  "Alist of indentation rules for CPerl mode.
The values mean:
  nil: do not indent;
  number: add this amount of indentation.

Not finished, not used.")

(defun cperl-where-am-i (&optional parse-start start-state)
  ;; Unfinished
  "Return a list of lists ((TYPE POS)...) of good points before the point.
POS may be nil if it is hard to find, say, when TYPE is `string' or `comment'.

Not finished, not used."
  (save-excursion
    (let* ((start-point (point))
	   (s-s (cperl-get-state))
	   (start (nth 0 s-s))
	   (state (nth 1 s-s))
	   (prestart (nth 3 s-s))
	   (containing-sexp (car (cdr state)))
	   (case-fold-search nil)
	   (res (list (list 'parse-start start) (list 'parse-prestart prestart))))
      (cond ((nth 3 state)		; In string
	     (setq res (cons (list 'string nil (nth 3 state)) res))) ; What started string
	    ((nth 4 state)		; In comment
	     (setq res (cons '(comment) res)))
	    ((null containing-sexp)
	     ;; Line is at top level.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (cperl-backward-to-noncomment (or parse-start (point-min)))
	     ;;(skip-chars-backward " \t\f\n")
	     (cond
	      ((or (bobp)
		   (memq (preceding-char) (append ";}" nil)))
	       (setq res (cons (list 'toplevel start) res)))
	      ((eq (preceding-char) ?\) )
	       (setq res (cons (list 'toplevel-after-parenth start) res)))
	      (t
	       (setq res (cons (list 'toplevel-continued start) res)))))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     ;; skip blanks if we do not close the expression.
	     (setq res (cons (list 'expression-blanks
				   (progn
				     (goto-char (1+ containing-sexp))
				     (or (looking-at "[ \t]*\\(#\\|$\\)")
					 (skip-chars-forward " \t"))
				     (point)))
			     (cons (list 'expression containing-sexp) res))))
	    ((progn
	      ;; Containing-expr starts with \{.  Check whether it is a hash.
	      (goto-char containing-sexp)
	      (not (cperl-block-p)))
	     (setq res (cons (list 'expression-blanks
				   (progn
				     (goto-char (1+ containing-sexp))
				     (or (looking-at "[ \t]*\\(#\\|$\\)")
					 (skip-chars-forward " \t"))
				     (point)))
			     (cons (list 'expression containing-sexp) res))))
	    (t
	     ;; Statement level.
	     (setq res (cons (list 'in-block containing-sexp) res))
	     ;; Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (cperl-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     ;; Back up comma-delimited lines too ?????
	     (while (or (eq (preceding-char) ?\,)
			(save-excursion (cperl-after-label)))
	       (if (eq (preceding-char) ?\,)
		   ;; Will go to beginning of line, essentially
		     ;; Will ignore embedded sexpr XXXX.
		   (cperl-backward-to-start-of-continued-exp containing-sexp))
	       (beginning-of-line)
	       (cperl-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) (append ";}{" '(nil)))) ; Was ?\,
		 ;; This line is continuation of preceding line's statement.
		 (list (list 'statement-continued containing-sexp))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
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
		    (while (progn (skip-chars-forward " \t\n" start-point)
				  (and (< (point) start-point)
				       (looking-at
					"#\\|[a-zA-Z_][a-zA-Z0-9_]*:[^:]")))
		      ;; Skip over comments and labels following openbrace.
		      (cond ((= (following-char) ?\#)
			     ;;(forward-line 1)
			     (end-of-line))
			    ;; label:
			    (t
			     (save-excursion (end-of-line)
					     (setq colon-line-end (point)))
			     (search-forward ":"))))
		    ;; Now at the point, after label, or at start
		    ;; of first statement in the block.
		    (and (< (point) start-point)
			 (if (> colon-line-end (point))
			     ;; Before statement after label
			     (if (> (current-indentation)
				    cperl-min-label-indent)
				 (list (list 'label-in-block (point)))
			       ;; Do not believe: `max' is involved
			       (list
				(list 'label-in-block-min-indent (point))))
			   ;; Before statement
			   (list 'statement-in-block (point))))))
		;; If no previous statement,
		;; indent it relative to line brace is on.
		;; For open brace in column zero, don't let statement
		;; start there too.  If cperl-indent-level is zero,
		;; use cperl-brace-offset + cperl-continued-statement-offset instead.
		;; For open-braces not the first thing in a line,
		;; add in cperl-brace-imaginary-offset.

		;; If first thing on a line:  ?????
		(+ (if (and (bolp) (zerop cperl-indent-level))
		       (+ cperl-brace-offset cperl-continued-statement-offset)
		     cperl-indent-level)
		   ;; Move back over whitespace before the openbrace.
		   ;; If openbrace is not first nonwhite thing on the line,
		   ;; add the cperl-brace-imaginary-offset.
		   (progn (skip-chars-backward " \t")
			  (if (bolp) 0 cperl-brace-imaginary-offset))
		   ;; If the openbrace is preceded by a parenthesized exp,
		   ;; move to the beginning of that;
		   ;; possibly a different line
		   (progn
		     (if (eq (preceding-char) ?\))
			 (forward-sexp -1))
		     ;; Get initial indentation of the line we are on.
		     ;; If line starts with label, calculate label indentation
		     (if (save-excursion
			   (beginning-of-line)
			   (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*:[^:]"))
			 (if (> (current-indentation) cperl-min-label-indent)
			     (- (current-indentation) cperl-label-offset)
			   (cperl-calculate-indent))
		       (current-indentation))))))))
      res)))

(defun cperl-calculate-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
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
Returns true if comment is found."
  (let (state stop-in cpoint (lim (progn (end-of-line) (point))))
      (beginning-of-line)
      (if (or
	   (eq (get-text-property (point) 'syntax-type) 'pod)
	   (re-search-forward "\\=[ \t]*\\(#\\|$\\)" lim t))
	  (if (eq (preceding-char) ?\#) (progn (backward-char 1) t))
	;; Else
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
			   "\\=\\w+[ \t]*#\\([^\n\\\\#]\\|\\\\[\\\\#]\\)*#\\([^\n\\\\#]\\|\\\\[\\\\#]\\)*"
			   lim 'move)
			  (setq stop-in t)))
		     ((looking-at "\\(m\\|q\\([qxwr]\\)?\\)\\>")
		      (or (re-search-forward
			   "\\=\\w+[ \t]*#\\([^\n\\\\#]\\|\\\\[\\\\#]\\)*#"
			   lim 'move)
			  (setq stop-in t)))
		     (t			; It was fair comment
		      (setq stop-in t)	; Finish
		      (goto-char (1- cpoint)))))
		(setq stop-in t)	; Finish
		(forward-char -1))
	    (setq stop-in t))		; Finish
	  )
	(nth 4 state))))

(defsubst cperl-1- (p)
  (max (point-min) (1- p)))

(defsubst cperl-1+ (p)
  (min (point-max) (1+ p)))

(defsubst cperl-modify-syntax-type (at how)
  (if (< at (point-max))
      (progn
	(put-text-property at (1+ at) 'syntax-table how)
	(put-text-property at (1+ at) 'rear-nonsticky t))))

(defun cperl-protect-defun-start (s e)
  ;; C code looks for "^\\s(" to skip comment backward in "hard" situations
  (save-excursion
    (goto-char s)
    (while (re-search-forward "^\\s(" e 'to-end)
      (put-text-property (1- (point)) (point) 'syntax-table cperl-st-punct))))

(defun cperl-commentify (bb e string &optional noface)
  (if cperl-use-syntax-table-text-property
      (if (eq noface 'n)		; Only immediate
	  nil
	;; We suppose that e is _after_ the end of construction, as after eol.
	(setq string (if string cperl-st-sfence cperl-st-cfence))
	(if (> bb (- e 2))
	    ;; one-char string/comment?!
	    (cperl-modify-syntax-type bb cperl-st-punct)
	  (cperl-modify-syntax-type bb string)
	  (cperl-modify-syntax-type (1- e) string))
	(if (and (eq string cperl-st-sfence) (> (- e 2) bb))
	    (put-text-property (1+ bb) (1- e)
			       'syntax-table cperl-string-syntax-table))
	(cperl-protect-defun-start bb e))
    ;; Fontify
    (or noface
	(not cperl-pod-here-fontify)
	(put-text-property bb e 'face (if string 'font-lock-string-face
					'font-lock-comment-face)))))

(defvar cperl-starters '(( ?\( . ?\) )
			 ( ?\[ . ?\] )
			 ( ?\{ . ?\} )
			 ( ?\< . ?\> )))

(defun cperl-forward-re (lim end is-2arg set-st st-l err-l argument
			     &optional ostart oend)
  ;; Works *before* syntax recognition is done
  ;; May modify syntax-type text property if the situation is too hard
  (let (b starter ender st i i2 go-forward reset-st)
    (skip-chars-forward " \t")
    ;; ender means matching-char matcher.
    (setq b (point)
	  starter (if (eobp) 0 (char-after b))
	  ender (cdr (assoc starter cperl-starters)))
    ;; What if starter == ?\\  ????
    (if set-st
	(if (car st-l)
	    (setq st (car st-l))
	  (setcar st-l (make-syntax-table))
	  (setq i 0 st (car st-l))
	  (while (< i 256)
	    (modify-syntax-entry i "." st)
	    (setq i (1+ i)))
	  (modify-syntax-entry ?\\ "\\" st)))
    (setq set-st t)
    ;; Whether we have an intermediate point
    (setq i nil)
    ;; Prepare the syntax table:
    (and set-st
	 (if (not ender)		; m/blah/, s/x//, s/x/y/
	     (modify-syntax-entry starter "$" st)
	   (modify-syntax-entry starter (concat "(" (list ender)) st)
	   (modify-syntax-entry ender  (concat ")" (list starter)) st)))
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
		   (= 0 (% (skip-chars-backward "\\\\") 2)))
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
		    (if (memq (following-char) '(?\  ?\t ?\n ?\f))
			(progn
			  (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
			      (goto-char (match-end 0))
			    (skip-chars-forward " \t\n\f"))
			  (setq i2 (point))))
		  (forward-char -1))
		(modify-syntax-entry starter (if (eq starter ?\\) "\\" ".") st)
		(if ender (modify-syntax-entry ender "." st))
		(setq set-st nil)
		(setq ender (cperl-forward-re lim end nil t st-l err-l
					      argument starter ender)
		 ender (nth 2 ender)))))
      (error (goto-char lim)
	     (setq set-st nil)
	     (if reset-st
		 (set-syntax-table reset-st))
	     (or end
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
    ;; i2: start of the second arg, if any (before delim iff `ender').
    ;; ender: the last arg bounded by parens-like chars, the second one of them
    ;; starter: the starting delimiter of the first arg
    ;; go-forward: has 2 args, and the second part is empty
    (list i i2 ender starter go-forward)))

(defsubst cperl-postpone-fontification (b e type val &optional now)
  ;; Do after syntactic fontification?
  (if cperl-syntaxify-by-font-lock
      (or now (put-text-property b e 'cperl-postpone (cons type val)))
      (put-text-property b e type val)))

;;; Here is how the global structures (those which cannot be
;;; recognized locally) are marked:
;;	a) PODs:
;;		Start-to-end is marked `in-pod' ==> t
;;		Each non-literal part is marked `syntax-type' ==> `pod'
;;		Each literal part is marked `syntax-type' ==> `in-pod'
;;	b) HEREs:
;;		Start-to-end is marked `here-doc-group' ==> t
;;		The body is marked `syntax-type' ==> `here-doc'
;;		The delimiter is marked `syntax-type' ==> `here-doc-delim'
;;	c) FORMATs:
;;		After-initial-line--to-end is marked `syntax-type' ==> `format'
;;	d) 'Q'uoted string:
;;		part between markers inclusive is marked `syntax-type' ==> `string'
;;		part between `q' and the first marker is marked `syntax-type' ==> `prestring'

(defun cperl-unwind-to-safe (before &optional end)
  ;; if BEFORE, go to the previous start-of-line on each step of unwinding
  (let ((pos (point)) opos)
    (setq opos pos)
    (while (and pos (get-text-property pos 'syntax-type))
      (setq pos (previous-single-property-change pos 'syntax-type))
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
	(progn
	  (while (and end (get-text-property end 'syntax-type))
	    (setq pos end
		  end (next-single-property-change end 'syntax-type)))
	  (or end pos)))))

(defvar cperl-nonoverridable-face)
(defvar font-lock-function-name-face)
(defvar font-lock-comment-face)

(defun cperl-find-pods-heres (&optional min max non-inter end ignore-max)
  "Scans the buffer for hard-to-parse Perl constructions.
If `cperl-pod-here-fontify' is not-nil after evaluation, will fontify
the sections using `cperl-pod-head-face', `cperl-pod-face',
`cperl-here-face'."
  (interactive)
  (or min (setq min (point-min)
		cperl-syntax-state nil
		cperl-syntax-done-to min))
  (or max (setq max (point-max)))
  (let* (face head-face here-face b e bb tag qtag b1 e1 argument i c tail tb
	      is-REx is-x-REx REx-comment-start REx-comment-end was-comment i2
	      (cperl-pod-here-fontify (eval cperl-pod-here-fontify)) go tmpend 
	      (case-fold-search nil) (inhibit-read-only t) (buffer-undo-list t)
	      (modified (buffer-modified-p))
	      (after-change-functions nil)
	      (use-syntax-state (and cperl-syntax-state
				     (>= min (car cperl-syntax-state))))
	      (state-point (if use-syntax-state
			       (car cperl-syntax-state)
			     (point-min)))
	      (state (if use-syntax-state
			 (cdr cperl-syntax-state)))
	      ;; (st-l '(nil)) (err-l '(nil)) ; Would overwrite - propagates from a function call to a function call!
	      (st-l (list nil)) (err-l (list nil))
	      ;; Somehow font-lock may be not loaded yet...
	      (font-lock-string-face (if (boundp 'font-lock-string-face)
					 font-lock-string-face
				       'font-lock-string-face))
	      (font-lock-constant-face (if (boundp 'font-lock-constant-face)
					 font-lock-constant-face
				       'font-lock-constant-face))
	      (font-lock-function-name-face
	       (if (boundp 'font-lock-function-name-face)
		   font-lock-function-name-face
		 'font-lock-function-name-face))
	      (font-lock-comment-face 
	       (if (boundp 'font-lock-comment-face)
		   font-lock-comment-face
		 'font-lock-comment-face))
	      (cperl-nonoverridable-face 
	       (if (boundp 'cperl-nonoverridable-face)
		   cperl-nonoverridable-face
		 'cperl-nonoverridable-face))
	      (stop-point (if ignore-max
			      (point-max)
			    max))
	      (search
	       (concat
		"\\(\\`\n?\\|^\n\\)=" 
		"\\|"
		;; One extra () before this:
		"<<"
		  "\\("			; 1 + 1
		  ;; First variant "BLAH" or just ``.
		     "[ \t]*"		; Yes, whitespace is allowed!
		     "\\([\"'`]\\)"	; 2 + 1 = 3
		     "\\([^\"'`\n]*\\)"	; 3 + 1
		     "\\3"
		  "\\|"
		  ;; Second variant: Identifier or \ID or empty
		    "\\\\?\\(\\([a-zA-Z_][a-zA-Z_0-9]*\\)?\\)" ; 4 + 1, 5 + 1
		    ;; Do not have <<= or << 30 or <<30 or << $blah.
		    ;; "\\([^= \t0-9$@%&]\\|[ \t]+[^ \t\n0-9$@%&]\\)" ; 6 + 1
		    "\\(\\)"		; To preserve count of pars :-( 6 + 1
		  "\\)"
		"\\|"
		;; 1+6 extra () before this:
		"^[ \t]*\\(format\\)[ \t]*\\([a-zA-Z0-9_]+\\)?[ \t]*=[ \t]*$"
		(if cperl-use-syntax-table-text-property
		    (concat
		     "\\|"
		     ;; 1+6+2=9 extra () before this:
		     "\\<\\(q[wxqr]?\\|[msy]\\|tr\\)\\>"
		     "\\|"
		     ;; 1+6+2+1=10 extra () before this:
		     "\\([?/<]\\)"	; /blah/ or ?blah? or <file*glob>
		     "\\|"
		     ;; 1+6+2+1+1=11 extra () before this:
		     "\\<sub\\>[ \t]*\\([a-zA-Z_:'0-9]+[ \t]*\\)?\\(([^()]*)\\)"
		     "\\|"
		     ;; 1+6+2+1+1+2=13 extra () before this:
		     "\\$\\(['{]\\)"
		     "\\|"
		     ;; 1+6+2+1+1+2+1=14 extra () before this:
		     "\\(\\<sub[ \t\n\f]+\\|[&*$@%]\\)[a-zA-Z0-9_]*'"
		     ;; 1+6+2+1+1+2+1+1=15 extra () before this:
		     "\\|"
		     "__\\(END\\|DATA\\)__"
		     ;; 1+6+2+1+1+2+1+1+1=16 extra () before this:
		     "\\|"
		     "\\\\\\(['`\"]\\)"
		     )
		  ""))))
    (unwind-protect
	(progn
	  (save-excursion
	    (or non-inter
		(message "Scanning for \"hard\" Perl constructions..."))
	    (and cperl-pod-here-fontify
		;; We had evals here, do not know why...
		(setq face cperl-pod-face
		      head-face cperl-pod-head-face
		      here-face cperl-here-face))
	    (remove-text-properties min max
				    '(syntax-type t in-pod t syntax-table t
						  cperl-postpone t
						  syntax-subtype t
						  rear-nonsticky t
						  indentable t))
	    ;; Need to remove face as well...
	    (goto-char min)
	    (and (eq system-type 'emx)
		 (looking-at "extproc[ \t]") ; Analogue of #!
		 (cperl-commentify min
				   (save-excursion (end-of-line) (point))
				   nil))
	    (while (and
		    (< (point) max)
		    (re-search-forward search max t))
	      (setq tmpend nil)		; Valid for most cases
	      (cond
	       ((match-beginning 1)	; POD section
		;;  "\\(\\`\n?\\|^\n\\)=" 
		(if (looking-at "cut\\>")
		    (if ignore-max
			nil		; Doing a chunk only
		      (message "=cut is not preceded by a POD section")
		      (or (car err-l) (setcar err-l (point))))
		  (beginning-of-line)

		  (setq b (point)
			bb b
			tb (match-beginning 0)
			b1 nil)		; error condition
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (or (re-search-forward "^\n=cut\\>" stop-point 'toend)
		      (progn
			(goto-char b)
			(if (re-search-forward "\n=cut\\>" stop-point 'toend)
			    (progn
			      (message "=cut is not preceded by an empty line")
			      (setq b1 t)
			      (or (car err-l) (setcar err-l b))))))
		  (beginning-of-line 2)	; An empty line after =cut is not POD!
		  (setq e (point))
		  (and (> e max)
		       (progn
			 (remove-text-properties 
			  max e '(syntax-type t in-pod t syntax-table t
					      cperl-postpone t
					      syntax-subtype t
					      rear-nonsticky t
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
		      (forward-char -1)))) ; Prepare for immediate pod start.
	       ;; Here document
	       ;; We do only one here-per-line
               ;; ;; One extra () before this:
	       ;;"<<"
	       ;;  "\\("			; 1 + 1
	       ;;  ;; First variant "BLAH" or just ``.
	       ;;     "\\([\"'`]\\)"	; 2 + 1
	       ;;     "\\([^\"'`\n]*\\)"	; 3 + 1
	       ;;     "\\3"
	       ;;  "\\|"
	       ;;  ;; Second variant: Identifier or \ID or empty
	       ;;    "\\\\?\\(\\([a-zA-Z_][a-zA-Z_0-9]*\\)?\\)" ; 4 + 1, 5 + 1
	       ;;    ;; Do not have <<= or << 30 or <<30 or << $blah.
	       ;;    ;; "\\([^= \t0-9$@%&]\\|[ \t]+[^ \t\n0-9$@%&]\\)" ; 6 + 1
	       ;;    "\\(\\)"		; To preserve count of pars :-( 6 + 1
	       ;;  "\\)"
	       ((match-beginning 2)	; 1 + 1
		;; Abort in comment:
		(setq b (point))
		(setq state (parse-partial-sexp state-point b nil nil state)
		      state-point b
		      tb (match-beginning 0)
		      i (or (nth 3 state) (nth 4 state)))
		(if i
		    (setq c t)
		  (setq c (and
			   (match-beginning 5)
			   (not (match-beginning 6)) ; Empty
			   (looking-at
			    "[ \t]*[=0-9$@%&(]"))))
		(if c			; Not here-doc
		    nil			; Skip it.
		  (if (match-beginning 5) ;4 + 1
		      (setq b1 (match-beginning 5) ; 4 + 1
			    e1 (match-end 5)) ; 4 + 1
		    (setq b1 (match-beginning 4) ; 3 + 1
			  e1 (match-end 4))) ; 3 + 1
		  (setq tag (buffer-substring b1 e1)
			qtag (regexp-quote tag))
		  (cond (cperl-pod-here-fontify
			 ;; Highlight the starting delimiter
			 (cperl-postpone-fontification b1 e1 'face font-lock-constant-face)
			 (cperl-put-do-not-fontify b1 e1 t)))
		  (forward-line)
		  (setq b (point))
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (cond ((re-search-forward (concat "^" qtag "$")
					    stop-point 'toend)
			 (if cperl-pod-here-fontify
			     (progn
			       ;; Highlight the ending delimiter
			       (cperl-postpone-fontification (match-beginning 0) (match-end 0)
						  'face font-lock-constant-face)
			       (cperl-put-do-not-fontify b (match-end 0) t)
			       ;; Highlight the HERE-DOC
			       (cperl-postpone-fontification b (match-beginning 0)
						  'face here-face)))
			 (setq e1 (cperl-1+ (match-end 0)))
			 (put-text-property b (match-beginning 0)
					    'syntax-type 'here-doc)
			 (put-text-property (match-beginning 0) e1
					    'syntax-type 'here-doc-delim)
			 (put-text-property b e1
					    'here-doc-group t)
			 (cperl-commentify b e1 nil)
			 (cperl-put-do-not-fontify b (match-end 0) t)
			 (if (> e1 max)
			     (setq tmpend tb)))
			(t (message "End of here-document `%s' not found." tag)
			   (or (car err-l) (setcar err-l b))))))
	       ;; format
	       ((match-beginning 8)
		;; 1+6=7 extra () before this:
		;;"^[ \t]*\\(format\\)[ \t]*\\([a-zA-Z0-9_]+\\)?[ \t]*=[ \t]*$"
		(setq b (point)
		      name (if (match-beginning 8) ; 7 + 1
			       (buffer-substring (match-beginning 8) ; 7 + 1
						 (match-end 8)) ; 7 + 1
			     "")
		      tb (match-beginning 0))
		(setq argument nil)
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
					   'face font-lock-string-face)
			(cperl-commentify b1 (point) nil)
			(cperl-put-do-not-fontify b1 (point) t))))
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (re-search-forward "^[.;]$" stop-point 'toend))
		(beginning-of-line)
		(if (looking-at "^\\.$")	; ";" is not supported yet
		    (progn
		      ;; Highlight the ending delimiter
		      (cperl-postpone-fontification (point) (+ (point) 2)
					 'face font-lock-string-face)
		      (cperl-commentify (point) (+ (point) 2) nil)
		      (cperl-put-do-not-fontify (point) (+ (point) 2) t))
		  (message "End of format `%s' not found." name)
		  (or (car err-l) (setcar err-l b)))
		(forward-line)
		(if (> (point) max)
		    (setq tmpend tb))
		(put-text-property b (point) 'syntax-type 'format))
	       ;; Regexp:
	       ((or (match-beginning 10) (match-beginning 11))
		;; 1+6+2=9 extra () before this:
		;; "\\<\\(q[wxqr]?\\|[msy]\\|tr\\)\\>"
		;; "\\|"
		;; "\\([?/<]\\)"	; /blah/ or ?blah? or <file*glob>
		(setq b1 (if (match-beginning 10) 10 11)
		      argument (buffer-substring
				(match-beginning b1) (match-end b1))
		      b (point)
		      i b
		      c (char-after (match-beginning b1))
		      bb (char-after (1- (match-beginning b1)))	; tmp holder
		      ;; bb == "Not a stringy"
		      bb (if (eq b1 10) ; user variables/whatever
			     (or
			      (memq bb '(?\$ ?\@ ?\% ?\* ?\#)) ; $#y
			      (and (eq bb ?-) (eq c ?s)) ; -s file test
			      (and (eq bb ?\&)
				   (not (eq (char-after  ; &&m/blah/
					     (- (match-beginning b1) 2))
					    ?\&))))
			   ;; <file> or <$file>
			   (and (eq c ?\<)
				;; Do not stringify <FH>, <$fh> :
				(save-match-data
				  (looking-at
				   "\\$?\\([_a-zA-Z:][_a-zA-Z0-9:]*\\)?>"))))
		      tb (match-beginning 0))
		(goto-char (match-beginning b1))
		(cperl-backward-to-noncomment (point-min))
		(or bb
		    (if (eq b1 11)	; bare /blah/ or ?blah? or <foo>
			(setq argument ""
			      bb	; Not a regexp?
			      (progn
				(not
				 ;; What is below: regexp-p?
				 (and
				  (or (memq (preceding-char)
					    (append (if (memq c '(?\? ?\<))
							;; $a++ ? 1 : 2
							"~{(=|&*!,;:"
						      "~{(=|&+-*!,;:") nil))
				      (and (eq (preceding-char) ?\})
					   (cperl-after-block-p (point-min)))
				      (and (eq (char-syntax (preceding-char)) ?w)
					   (progn
					     (forward-sexp -1)
;; After these keywords `/' starts a RE.  One should add all the
;; functions/builtins which expect an argument, but ...
					     (if (eq (preceding-char) ?-)
						 ;; -d ?foo? is a RE
						 (looking-at "[a-zA-Z]\\>")
					       (and
						(not (memq (preceding-char)
							   '(?$ ?@ ?& ?%)))
						(looking-at
						 "\\(while\\|if\\|unless\\|until\\|and\\|or\\|not\\|xor\\|split\\|grep\\|map\\|print\\)\\>")))))
				      (and (eq (preceding-char) ?.)
					   (eq (char-after (- (point) 2)) ?.))
				      (bobp))
				  ;;  m|blah| ? foo : bar;
				  (not
				   (and (eq c ?\?)
					cperl-use-syntax-table-text-property
					(not (bobp))
					(progn
					  (forward-char -1)
					  (looking-at "\\s|")))))))
			      b (1- b))
		      ;; s y tr m
		      ;; Check for $a->y
		      (if (and (eq (preceding-char) ?>)
			       (eq (char-after (- (point) 2)) ?-))
			  ;; Not a regexp
			  (setq bb t))))
		(or bb (setq state (parse-partial-sexp
				    state-point b nil nil state)
			     state-point b))
		(goto-char b)
		(if (or bb (nth 3 state) (nth 4 state))
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
		  (setq b (point)
			;; has 2 args
			i2 (string-match "^\\([sy]\\|tr\\)$" argument)
			;; We do not search to max, since we may be called from
			;; some hook of fontification, and max is random
			i (cperl-forward-re stop-point end
					    i2
					    t st-l err-l argument)
			;; Note that if `go', then it is considered as 1-arg
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
			is-x-REx nil)	; REx has //x modifier
		  ;; Commenting \\ is dangerous, what about ( ?
		  (and i tail
		       (eq (char-after i) ?\\)
		       (setq qtag t))
		  (if (looking-at "\\sw*x") ; qr//x
		      (setq is-x-REx t))
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
			  (and
			   ;; silent:
			   (cperl-find-pods-heres b1 (1- (point)) t end)
			   ;; Error
			   (goto-char (1+ max)))
			  (if (and tag (eq (preceding-char) ?\>))
			      (progn
				(cperl-modify-syntax-type (1- (point)) cperl-st-ket)
				(cperl-modify-syntax-type i cperl-st-bra)))
			  (put-text-property b i 'syntax-type 'string)
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
			(forward-word 1) ; skip modifiers s///s
			(if tail (cperl-commentify tail (point) t))
			(cperl-postpone-fontification 
			 e1 (point) 'face 'cperl-nonoverridable-face)))
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
		       b e 'face font-lock-function-name-face)
		    (if (or i2		; Has 2 args
			    (and cperl-fontify-m-as-s
				 (or
				  (string-match "^\\(m\\|qr\\)$" argument)
				  (and (eq 0 (length argument))
				       (not (eq ?\< (char-after b)))))))
			(progn
			  (cperl-postpone-fontification
			   b (cperl-1+ b) 'face font-lock-constant-face)
			  (cperl-postpone-fontification
			   (1- e) e 'face font-lock-constant-face)))
		    (if (and is-REx cperl-regexp-scan)
			;; Process RExen better
			(save-excursion
			  (goto-char (1+ b))
			  (while
			      (and (< (point) e)
				   (re-search-forward
				    (if is-x-REx
					(if (eq (char-after b) ?\#)
					    "\\((\\?\\\\#\\)\\|\\(\\\\#\\)"
					    "\\((\\?#\\)\\|\\(#\\)")
					(if (eq (char-after b) ?\#)
					    "\\((\\?\\\\#\\)"
					  "\\((\\?#\\)"))
				    (1- e) 'to-end))
			    (goto-char (match-beginning 0))
			    (setq REx-comment-start (point)
				  was-comment t)
			    (if (save-excursion
				  (and
				   ;; XXX not working if outside delimiter is #
				   (eq (preceding-char) ?\\)
				   (= (% (skip-chars-backward "$\\\\") 2) -1)))
				;; Not a comment, avoid loop:
				(progn (setq was-comment nil)
				       (forward-char 1))
			      (if (match-beginning 2)
				  (progn 
				    (beginning-of-line 2)
				    (if (> (point) e)
					(goto-char (1- e))))
				;; Works also if the outside delimiters are ().
				(or (search-forward ")" (1- e) 'toend)
				    (message
				     "Couldn't find end of (?#...)-comment in a REx, pos=%s"
				     REx-comment-start))))
			    (if (>= (point) e)
				(goto-char (1- e)))
			    (if was-comment
				(progn
				  (setq REx-comment-end (point))
				  (cperl-commentify
				   REx-comment-start REx-comment-end nil)
				  (cperl-postpone-fontification 
				   REx-comment-start REx-comment-end
				   'face font-lock-comment-face))))))
		    (if (and is-REx is-x-REx)
			(put-text-property (1+ b) (1- e) 
					   'syntax-subtype 'x-REx)))
		  (if i2
		      (progn
			(cperl-postpone-fontification
			 (1- e1) e1 'face font-lock-constant-face)
			(if (assoc (char-after b) cperl-starters)
			    (cperl-postpone-fontification
			     b1 (1+ b1) 'face font-lock-constant-face))))
		  (if (> (point) max)
		      (setq tmpend tb))))
	       ((match-beginning 13)	; sub with prototypes
		(setq b (match-beginning 0))
		(if (memq (char-after (1- b))
			  '(?\$ ?\@ ?\% ?\& ?\*))
		    nil
		  (setq state (parse-partial-sexp
			       state-point b nil nil state)
			state-point b)
		  (if (or (nth 3 state) (nth 4 state))
		      nil
		    ;; Mark as string
		    (cperl-commentify (match-beginning 13) (match-end 13) t))
		  (goto-char (match-end 0))))
	       ;; 1+6+2+1+1+2=13 extra () before this:
	       ;;    "\\$\\(['{]\\)"
	       ((and (match-beginning 14)
		     (eq (preceding-char) ?\')) ; $'
		(setq b (1- (point))
		      state (parse-partial-sexp
			     state-point (1- b) nil nil state)
		      state-point (1- b))
		(if (nth 3 state)	; in string
		    (cperl-modify-syntax-type (1- b) cperl-st-punct))
		(goto-char (1+ b)))
	       ;; 1+6+2+1+1+2=13 extra () before this:
	       ;;    "\\$\\(['{]\\)"
	       ((match-beginning 14)	; ${
		(setq bb (match-beginning 0))
		(cperl-modify-syntax-type bb cperl-st-punct))
	       ;; 1+6+2+1+1+2+1=14 extra () before this:
	       ;;    "\\(\\<sub[ \t\n\f]+\\|[&*$@%]\\)[a-zA-Z0-9_]*'")
	       ((match-beginning 15)	; old $abc'efg syntax
		(setq bb (match-end 0)
		      b (match-beginning 0)
		      state (parse-partial-sexp
			     state-point b nil nil state)
		      state-point b)
		(if (nth 3 state)	; in string
		    nil
		  (put-text-property (1- bb) bb 'syntax-table cperl-st-word))
		(goto-char bb))
	       ;; 1+6+2+1+1+2+1+1=15 extra () before this:
	       ;; "__\\(END\\|DATA\\)__"
	       ((match-beginning 16)	; __END__, __DATA__
		(setq bb (match-end 0)
		      b (match-beginning 0)
		      state (parse-partial-sexp
			     state-point b nil nil state)
		      state-point b)
		(if (or (nth 3 state) (nth 4 state))
		    nil
		  ;; (put-text-property b (1+ bb) 'syntax-type 'pod) ; Cheat
		  (cperl-commentify b bb nil)
		  (setq end t))
		(goto-char bb))
	       ((match-beginning 17)	; "\\\\\\(['`\"]\\)"
		(setq bb (match-end 0)
		      b (match-beginning 0))
		(goto-char b)
		(skip-chars-backward "\\\\")
		;;;(setq i2 (= (% (skip-chars-backward "\\\\") 2) -1))
		(setq state (parse-partial-sexp 
			     state-point b nil nil state)
		      state-point b)
		(if (or (nth 3 state) (nth 4 state) )
		    nil
		  (cperl-modify-syntax-type b cperl-st-punct))
		(goto-char bb))
	       (t (error "Error in regexp of the sniffer")))
	      (if (> (point) stop-point)
		  (progn
		    (if end
			(message "Garbage after __END__/__DATA__ ignored")
		      (message "Unbalanced syntax found while scanning")
		      (or (car err-l) (setcar err-l b)))
		    (goto-char stop-point))))
	    (setq cperl-syntax-state (cons state-point state)
		  cperl-syntax-done-to (or tmpend (max (point) max))))
	  (if (car err-l) (goto-char (car err-l))
	    (or non-inter
		(message "Scanning for \"hard\" Perl constructions... done"))))
      (and (buffer-modified-p)
	   (not modified)
	   (set-buffer-modified-p nil))
      (set-syntax-table cperl-mode-syntax-table))
    (car err-l)))

(defun cperl-backward-to-noncomment (lim)
  ;; Stops at lim or after non-whitespace that is not in comment
  (let (stop p pr)
    (while (and (not stop) (> (point) (or lim 1)))
      (skip-chars-backward " \t\n\f" lim)
      (setq p (point))
      (beginning-of-line)
      (if (memq (setq pr (get-text-property (point) 'syntax-type))
		'(pod here-doc here-doc-delim))
	  (cperl-unwind-to-safe nil)
      (if (or (looking-at "^[ \t]*\\(#\\|$\\)")
	      (progn (cperl-to-comment-or-eol) (bolp)))
	  nil	; Only comment, skip
	;; Else
	(skip-chars-backward " \t")
	(if (< p (point)) (goto-char p))
	  (setq stop t))))))

(defun cperl-after-block-p (lim)
  ;; We suppose that the preceding char is }.
  (save-excursion
    (condition-case nil
	(progn
	  (forward-sexp -1)
	  (cperl-backward-to-noncomment lim)
	  (or (eq (point) lim)
	      (eq (preceding-char) ?\) ) ; if () {}    sub f () {}
	      (if (eq (char-syntax (preceding-char)) ?w) ; else {}
		  (save-excursion
		    (forward-sexp -1)
		    (or (looking-at "\\(else\\|grep\\|map\\|BEGIN\\|END\\|CHECK\\|INIT\\)\\>")
			;; sub f {}
			(progn
			  (cperl-backward-to-noncomment lim)
			  (and (eq (char-syntax (preceding-char)) ?w)
			       (progn
				 (forward-sexp -1)
				 (looking-at "sub\\>"))))))
		(cperl-after-expr-p lim))))
      (error nil))))

(defun cperl-after-expr-p (&optional lim chars test)
  "Return true if the position is good for start of expression.
TEST is the expression to evaluate at the found position.  If absent,
CHARS is a string that contains good characters to have before us (however,
`}' is treated \"smartly\" if it is not in the list)."
  (let (stop p
	     (lim (or lim (point-min))))
    (save-excursion
      (while (and (not stop) (> (point) lim))
	(skip-chars-backward " \t\n\f" lim)
	(setq p (point))
	(beginning-of-line)
	(if (looking-at "^[ \t]*\\(#\\|$\\)") nil ; Only comment, skip
	  ;; Else: last iteration, or a label
	  (cperl-to-comment-or-eol)
	  (skip-chars-backward " \t")
	  (if (< p (point)) (goto-char p))
	  (setq p (point))
	  (if (and (eq (preceding-char) ?:)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n\f" lim)
		     (eq (char-syntax (preceding-char)) ?w)))
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
		       (cperl-after-block-p lim)))))))))

(defun cperl-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) (append ")]}\"'`" nil))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun cperl-after-block-and-statement-beg (lim)
  ;; We assume that we are after ?\}
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
	     "\\(map\\|grep\\|printf?\\|system\\|exec\\|tr\\|s\\)\\>")))))))


(defun cperl-indent-exp ()
  "Simple variant of indentation of continued-sexp.

Will not indent comment if it starts at `comment-indent' or looks like
continuation of the comment on the previous line.

If `cperl-indent-region-fix-constructs', will improve spacing on
conditional/loop constructs."
  (interactive)
  (save-excursion
    (let ((tmp-end (progn (end-of-line) (point))) top done)
      (save-excursion
	(beginning-of-line)
	(while (null done)
	  (setq top (point))
	  (while (= (nth 0 (parse-partial-sexp (point) tmp-end
					       -1)) -1)
	    (setq top (point)))		; Get the outermost parenths in line
	  (goto-char top)
	  (while (< (point) tmp-end)
	    (parse-partial-sexp (point) tmp-end nil t) ; To start-sexp or eol
	    (or (eolp) (forward-sexp 1)))
	  (if (> (point) tmp-end)
	      (save-excursion
		(end-of-line)
		(setq tmp-end (point)))
	    (setq done t)))
	(goto-char tmp-end)
	(setq tmp-end (point-marker)))
      (if cperl-indent-region-fix-constructs
	  (cperl-fix-line-spacing tmp-end))
      (cperl-indent-region (point) tmp-end))))

(defun cperl-fix-line-spacing (&optional end parse-data)
  "Improve whitespace in a conditional/loop construct.
Returns some position at the last line."
  (interactive)
  (or end
      (setq end (point-max)))
  (let (p pp ml have-brace ret
	  (ee (save-excursion (end-of-line) (point)))
	  (cperl-indent-region-fix-constructs
	   (or cperl-indent-region-fix-constructs 1)))
    (save-excursion
      (beginning-of-line)
      (setq ret (point))
      ;;  }? continue
      ;;  blah; }
      (if (not
	   (or (looking-at "[ \t]*\\(els\\(e\\|if\\)\\|continue\\|if\\|while\\|for\\(each\\)?\\|until\\)")
	       (setq have-brace (save-excursion (search-forward "}" ee t)))))
	  nil				; Do not need to do anything
      ;; Looking at:
      ;; }
      ;; else
      (if (and cperl-merge-trailing-else
	       (looking-at
		"[ \t]*}[ \t]*\n[ \t\n]*\\(els\\(e\\|if\\)\\|continue\\)\\>"))
	  (progn
	    (search-forward "}")
	    (setq p (point))
	    (skip-chars-forward " \t\n")
	    (delete-region p (point))
	    (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	    (beginning-of-line)))
      ;; Looking at:
      ;; }     else
      (if (looking-at "[ \t]*}\\(\t*\\|[ \t][ \t]+\\)\\<\\(els\\(e\\|if\\)\\|continue\\)\\>")
	  (progn
	    (search-forward "}")
	    (delete-horizontal-space)
	    (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	    (beginning-of-line)))
      ;; Looking at:
      ;; else   {
      (if (looking-at
	   "[ \t]*}?[ \t]*\\<\\(\\els\\(e\\|if\\)\\|continue\\|unless\\|if\\|while\\|for\\(each\\)?\\|until\\)\\>\\(\t*\\|[ \t][ \t]+\\)[^ \t\n#]")
	  (progn
	    (forward-word 1)
	    (delete-horizontal-space)
	    (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	    (beginning-of-line)))
      ;; Looking at:
      ;; foreach my    $var
      (if (looking-at 
	   "[ \t]*\\<for\\(each\\)?[ \t]+\\(my\\|local\\|our\\)\\(\t*\\|[ \t][ \t]+\\)[^ \t\n]")
	  (progn
	    (forward-word 2)
	    (delete-horizontal-space)
	    (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	    (beginning-of-line)))
      ;; Looking at:
      ;; foreach my $var     (
      (if (looking-at 
	     "[ \t]*\\<for\\(each\\)?[ \t]+\\(my\\|local\\|our\\)[ \t]*\\$[_a-zA-Z0-9]+\\(\t*\\|[ \t][ \t]+\\)[^ \t\n#]")
	  (progn
	    (forward-word 3)
	    (delete-horizontal-space)
	    (insert
	     (make-string cperl-indent-region-fix-constructs ?\ ))
	    (beginning-of-line)))
      ;; Looking at:
      ;; } foreach my $var ()    {
      (if (looking-at 
	     "[ \t]*\\(}[ \t]*\\)?\\<\\(\\els\\(e\\|if\\)\\|continue\\|if\\|unless\\|while\\|for\\(each\\)?\\(\\([ t]+\\(my\\|local\\|our\\)\\)?[ \t]*\\$[_a-zA-Z0-9]+\\)?\\|until\\)\\>\\([ \t]*(\\|[ \t\n]*{\\)\\|[ \t]*{")
	  (progn
	    (setq ml (match-beginning 8))
	    (re-search-forward "[({]")
	    (forward-char -1)
	    (setq p (point))
	    (if (eq (following-char) ?\( )
		(progn
		  (forward-sexp 1)
		  (setq pp (point)))
	      ;; after `else' or nothing
	      (if ml	; after `else'
		  (skip-chars-backward " \t\n")
		(beginning-of-line))
	      (setq pp nil))
	    ;; Now after the sexp before the brace
	    ;; Multiline expr should be special
	    (setq ml (and pp (save-excursion (goto-char p)
					     (search-forward "\n" pp t))))
	    (if (and (or (not pp) (< pp end))
		     (looking-at "[ \t\n]*{"))
		(progn
		  (cond
		   ((bolp)		; Were before `{', no if/else/etc
		    nil)
		   ((looking-at "\\(\t*\\| [ \t]+\\){")
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
		       (make-string cperl-indent-region-fix-constructs ?\ ))))
		   ((and (looking-at "[ \t]*\n")
			 (not (if ml
				  cperl-extra-newline-before-brace-multiline
				cperl-extra-newline-before-brace)))
		    (setq pp (point))
		    (skip-chars-forward " \t\n")
		    (delete-region pp (point))
		    (insert
		     (make-string cperl-indent-region-fix-constructs ?\ ))))
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
      (setq p (point) pp (save-excursion (end-of-line) (point))) ; May be different from ee.
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
(defun cperl-delay-update-hook (beg end old-len)
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
  (cperl-update-syntaxification end end)
  (save-excursion
    (let (cperl-update-start cperl-update-end (h-a-c after-change-functions))
      (let (st comm old-comm-indent new-comm-indent p pp i empty
	       (indent-info (if cperl-emacs-can-parse
				(list nil nil nil) ; Cannot use '(), since will modify
			      nil))
	       after-change-functions	; Speed it up!
	     (pm 0) (imenu-scanning-message "Indenting... (%3d%%)"))
	(if h-a-c (add-hook 'after-change-functions 'cperl-delay-update-hook))
      (goto-char start)
      (setq old-comm-indent (and (cperl-to-comment-or-eol)
				 (current-column))
	    new-comm-indent old-comm-indent)
      (goto-char start)
      (setq end (set-marker (make-marker) end))	; indentation changes pos
      (or (bolp) (beginning-of-line 2))
      (or (fboundp 'imenu-progress-message)
	  (message "Indenting... For feedback load `imenu'..."))
      (while (and (<= (point) end) (not (eobp))) ; bol to check start
	(and (fboundp 'imenu-progress-message)
	     (imenu-progress-message
	      pm (/ (* 100 (- (point) start)) (- end start -1))))
	  (setq st (point))
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
	(beginning-of-line 2))
      	(if (fboundp 'imenu-progress-message)
	     (imenu-progress-message pm 100)
	  (message nil)))
      ;; Now run the update hooks
      (if after-change-functions
	  (save-excursion
	    (if cperl-update-end
		(progn
		  (goto-char cperl-update-end)
		  (insert " ")
		  (delete-char -1)
		  (goto-char cperl-update-start)
		  (insert " ")
		  (delete-char -1))))))))

;; Stolen from lisp-mode with a lot of improvements

(defun cperl-fill-paragraph (&optional justify iteration)
  "Like \\[fill-paragraph], but handle CPerl comments.
If any of the current line is a comment, fill the comment or the
block of it that point is in, preserving the comment's initial
indentation and initial hashes.  Behaves usually outside of comment."
  (interactive "P")
  (let (
	;; Non-nil if the current line contains a comment.
	has-comment

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
	      (concat (make-string (current-column) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))
	      spaces (progn (skip-chars-backward " \t")
			    (buffer-substring (point) start))
	      dc (- c (current-column)) len (- start (point))
	      start (point-marker))
	(delete-char len)
	(insert (make-string dc ?-)))))
    (if (not has-comment)
	(fill-paragraph justify)	; Do the usual thing outside of comment
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
	(while (progn (forward-line 1) (< (point) (point-max)))
	  (skip-chars-forward " \t")
	  (and (looking-at "#+")
	       (delete-char (- (match-end 0) (match-beginning 0)))))

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
	      (cperl-fill-paragraph justify t)))))))

(defun cperl-do-auto-fill ()
  ;; Break out if the line is short enough
  (if (> (save-excursion
	   (end-of-line)
	   (current-column))
	 fill-column)
  (let ((c (save-excursion (beginning-of-line)
			   (cperl-to-comment-or-eol) (point)))
	(s (memq (following-char) '(?\ ?\t))) marker)
    (if (>= c (point)) nil
      (setq marker (point-marker))
      (cperl-fill-paragraph)
      (goto-char marker)
      ;; Is not enough, sometimes marker is a start of line
      (if (bolp) (progn (re-search-forward "#+[ \t]*")
			(goto-char (match-end 0))))
      ;; Following space could have gone:
      (if (or (not s) (memq (following-char) '(?\ ?\t))) nil
	(insert " ")
	(backward-char 1))
      ;; Previous space could have gone:
      (or (memq (preceding-char) '(?\ ?\t)) (insert " "))))))

(defvar cperl-imenu--function-name-regexp-perl
  (concat
   "^\\("
       "[ \t]*\\(sub\\|package\\)[ \t\n]+\\([a-zA-Z_0-9:']+\\)[ \t]*\\(([^()]*)[ \t]*\\)?"
     "\\|"
       "=head\\([12]\\)[ \t]+\\([^\n]+\\)$"
   "\\)"))

(defun cperl-imenu-addback (lst &optional isback name)
  ;; We suppose that the lst is a DAG, unless the first element only
  ;; loops back, and ISBACK is set.  Thus this function cannot be
  ;; applied twice without ISBACK set.
  (cond ((not cperl-imenu-addback) lst)
	(t
	 (or name
	     (setq name "+++BACK+++"))
	 (mapcar (function (lambda (elt)
			     (if (and (listp elt) (listp (cdr elt)))
				 (progn
				   ;; In the other order it goes up
				   ;; one level only ;-(
				   (setcdr elt (cons (cons name lst)
						     (cdr elt)))
				   (cperl-imenu-addback (cdr elt) t name)
				   ))))
		 (if isback (cdr lst) lst))
	 lst)))

(defun cperl-imenu--create-perl-index (&optional regexp)
  (require 'imenu)			; May be called from TAGS creator
  (let ((index-alist '()) (index-pack-alist '()) (index-pod-alist '())
	(index-unsorted-alist '()) (i-s-f (default-value 'imenu-sort-function))
	(index-meth-alist '()) meth
	packages ends-ranges p marker
	(prev-pos 0) char fchar index index1 name (end-range 0) package)
    (goto-char (point-min))
    (if noninteractive
	(message "Scanning Perl for index")
      (imenu-progress-message prev-pos 0))
    (cperl-update-syntaxification (point-max) (point-max))
    ;; Search for the function
    (progn ;;save-match-data
      (while (re-search-forward
	      (or regexp cperl-imenu--function-name-regexp-perl)
	      nil t)
	(or noninteractive
	    (imenu-progress-message prev-pos))
	(cond
	 ((and				; Skip some noise if building tags
	   (match-beginning 2)		; package or sub
	   (eq (char-after (match-beginning 2)) ?p) ; package
	   (not (save-match-data
		  (looking-at "[ \t\n]*;"))))  ; Plain text word 'package'
	  nil)
	 ((and
	   (match-beginning 2)		; package or sub
	   ;; Skip if quoted (will not skip multi-line ''-strings :-():
	   (null (get-text-property (match-beginning 1) 'syntax-table))
	   (null (get-text-property (match-beginning 1) 'syntax-type))
	   (null (get-text-property (match-beginning 1) 'in-pod)))
	  (save-excursion
	    (goto-char (match-beginning 2))
	    (setq fchar (following-char))
	    )
	  ;; (if (looking-at "([^()]*)[ \t\n\f]*")
	  ;;    (goto-char (match-end 0)))	; Messes what follows
	  (setq char (following-char)	; ?\; for "sub foo () ;"
		meth nil
		p (point))
	  (while (and ends-ranges (>= p (car ends-ranges)))
	    ;; delete obsolete entries
	    (setq ends-ranges (cdr ends-ranges) packages (cdr packages)))
	  (setq package (or (car packages) "")
		end-range (or (car ends-ranges) 0))
	  (if (eq fchar ?p)
	      (setq name (buffer-substring (match-beginning 3) (match-end 3))
		    name (progn
			   (set-text-properties 0 (length name) nil name)
			   name)
		    package (concat name "::")
		    name (concat "package " name)
		    end-range
		    (save-excursion
		      (parse-partial-sexp (point) (point-max) -1) (point))
		    ends-ranges (cons end-range ends-ranges)
		    packages (cons package packages)))
	  ;;   )
	  ;; Skip this function name if it is a prototype declaration.
	  (if (and (eq fchar ?s) (eq char ?\;)) nil
	    (setq name (buffer-substring (match-beginning 3) (match-end 3))
		  marker (make-marker))
	    (set-text-properties 0 (length name) nil name)
	    (set-marker marker (match-end 3))
	    (if (eq fchar ?p) 
		(setq name (concat "package " name))
	      (cond ((string-match "[:']" name)
		     (setq meth t))
		    ((> p end-range) nil)
		    (t
		     (setq name (concat package name) meth t))))
	    (setq index (cons name marker))
	    (if (eq fchar ?p) 
		(push index index-pack-alist)
	      (push index index-alist))
	    (if meth (push index index-meth-alist))
	    (push index index-unsorted-alist)))
	 ((match-beginning 5)		; Pod section
	  ;; (beginning-of-line)
	  (setq index (imenu-example--name-and-position)
		name (buffer-substring (match-beginning 6) (match-end 6)))
	  (set-text-properties 0 (length name) nil name)
	  (if (eq (char-after (match-beginning 5)) ?2)
	      (setq name (concat "   " name)))
	  (setcar index name)
	  (setq index1 (cons (concat "=" name) (cdr index)))
	  (push index index-pod-alist)
	  (push index1 index-unsorted-alist)))))
    (or noninteractive
	(imenu-progress-message prev-pos 100))
    (setq index-alist
	  (if (default-value 'imenu-sort-function)
	      (sort index-alist (default-value 'imenu-sort-function))
	      (nreverse index-alist)))
    (and index-pod-alist
	 (push (cons "+POD headers+..."
		     (nreverse index-pod-alist))
	       index-alist))
    (and (or index-pack-alist index-meth-alist)
	 (let ((lst index-pack-alist) hier-list pack elt group name)
	   ;; Remove "package ", reverse and uniquify.
	   (while lst
	     (setq elt (car lst) lst (cdr lst) name (substring (car elt) 8))
	     (if (assoc name hier-list) nil
	       (setq hier-list (cons (cons name (cdr elt)) hier-list))))
	   (setq lst index-meth-alist)
	   (while lst
	     (setq elt (car lst) lst (cdr lst))
	     (cond ((string-match "\\(::\\|'\\)[_a-zA-Z0-9]+$" (car elt))
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
    (and index-pack-alist
	 (push (cons "+Packages+..."
		     (nreverse index-pack-alist))
	       index-alist))
    (and (or index-pack-alist index-pod-alist
	     (default-value 'imenu-sort-function))
	 index-unsorted-alist
	 (push (cons "+Unsorted List+..."
		     (nreverse index-unsorted-alist))
	       index-alist))
    (cperl-imenu-addback index-alist)))


(defvar cperl-outline-regexp
  (concat cperl-imenu--function-name-regexp-perl "\\|" "\\`"))

;; Suggested by Mark A. Hershberger
(defun cperl-outline-level ()
  (looking-at outline-regexp)
  (cond ((not (match-beginning 1)) 0)	; beginning-of-file
	((match-beginning 2)
	 (if (eq (char-after (match-beginning 2)) ?p)
	     0				; package
	   1))				; sub
	((match-beginning 5)
	 (if (eq (char-after (match-beginning 5)) ?1)
	     1				; head1
	   2))				; head2
	(t 3)))				; should not happen


(defvar cperl-compilation-error-regexp-alist 
  ;; This look like a paranoiac regexp: could anybody find a better one? (which WORK).
  '(("^[^\n]* \\(file\\|at\\) \\([^ \t\n]+\\) [^\n]*line \\([0-9]+\\)[\\., \n]"
     2 3))
  "Alist that specifies how to match errors in perl output.")

(if (fboundp 'eval-after-load)
    (eval-after-load
     "mode-compile"
     '(setq perl-compilation-error-regexp-alist
	   cperl-compilation-error-regexp-alist)))


(defun cperl-windowed-init ()
  "Initialization under windowed version."
  (if (or (featurep 'ps-print) cperl-faces-init)
      ;; Need to init anyway:
      (or cperl-faces-init (cperl-init-faces))
    (add-hook 'font-lock-mode-hook
	      (function
	       (lambda ()
		 (if (memq major-mode '(perl-mode cperl-mode))
		     (progn
		       (or cperl-faces-init (cperl-init-faces)))))))
    (if (fboundp 'eval-after-load)
	(eval-after-load
	 "ps-print"
	 '(or cperl-faces-init (cperl-init-faces))))))

(defvar cperl-font-lock-keywords-1 nil
  "Additional expressions to highlight in Perl mode.  Minimal set.")
(defvar cperl-font-lock-keywords nil
  "Additional expressions to highlight in Perl mode.  Default set.")
(defvar cperl-font-lock-keywords-2 nil
  "Additional expressions to highlight in Perl mode.  Maximal set")

(defun cperl-load-font-lock-keywords ()
  (or cperl-faces-init (cperl-init-faces))
  cperl-font-lock-keywords)

(defun cperl-load-font-lock-keywords-1 ()
  (or cperl-faces-init (cperl-init-faces))
  cperl-font-lock-keywords-1)

(defun cperl-load-font-lock-keywords-2 ()
  (or cperl-faces-init (cperl-init-faces))
  cperl-font-lock-keywords-2)

(defun cperl-init-faces-weak ()
  ;; Allow `cperl-find-pods-heres' to run.
  (or (boundp 'font-lock-constant-face)
      (cperl-force-face font-lock-constant-face
                        "Face for constant and label names")
      ;;(setq font-lock-constant-face 'font-lock-constant-face)
      ))

(defun cperl-init-faces ()
  (condition-case errs
      (progn
	(require 'font-lock)
	(and (fboundp 'font-lock-fontify-anchored-keywords)
	     (featurep 'font-lock-extra)
	     (message "You have an obsolete package `font-lock-extra'.  Install `choose-color'."))
	(let (t-font-lock-keywords t-font-lock-keywords-1 font-lock-anchored)
	  (if (fboundp 'font-lock-fontify-anchored-keywords)
	      (setq font-lock-anchored t))
	  (setq
	   t-font-lock-keywords
	   (list
	    `("[ \t]+$" 0 ',cperl-invalid-face t)
	    (cons
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      (mapconcat
	       'identity
	       '("if" "until" "while" "elsif" "else" "unless" "for"
		 "foreach" "continue" "exit" "die" "last" "goto" "next"
		 "redo" "return" "local" "exec" "sub" "do" "dump" "use"
		 "require" "package" "eval" "my" "BEGIN" "END" "CHECK" "INIT")
	       "\\|")			; Flow control
	      "\\)\\>") 2)		; was "\\)[ \n\t;():,\|&]"
					; In what follows we use `type' style
					; for overwritable builtins
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      ;; "CORE" "__FILE__" "__LINE__" "abs" "accept" "alarm"
	      ;; "and" "atan2" "bind" "binmode" "bless" "caller"
	      ;; "chdir" "chmod" "chown" "chr" "chroot" "close"
	      ;; "closedir" "cmp" "connect" "continue" "cos" "crypt"
	      ;; "dbmclose" "dbmopen" "die" "dump" "endgrent"
	      ;; "endhostent" "endnetent" "endprotoent" "endpwent"
	      ;; "endservent" "eof" "eq" "exec" "exit" "exp" "fcntl"
	      ;; "fileno" "flock" "fork" "formline" "ge" "getc"
	      ;; "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
	      ;; "gethostbyname" "gethostent" "getlogin"
	      ;; "getnetbyaddr" "getnetbyname" "getnetent"
	      ;; "getpeername" "getpgrp" "getppid" "getpriority"
	      ;; "getprotobyname" "getprotobynumber" "getprotoent"
	      ;; "getpwent" "getpwnam" "getpwuid" "getservbyname"
	      ;; "getservbyport" "getservent" "getsockname"
	      ;; "getsockopt" "glob" "gmtime" "gt" "hex" "index" "int"
	      ;; "ioctl" "join" "kill" "lc" "lcfirst" "le" "length"
	      ;; "link" "listen" "localtime" "lock" "log" "lstat" "lt"
	      ;; "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "ne"
	      ;; "not" "oct" "open" "opendir" "or" "ord" "pack" "pipe"
	      ;; "quotemeta" "rand" "read" "readdir" "readline"
	      ;; "readlink" "readpipe" "recv" "ref" "rename" "require"
	      ;; "reset" "reverse" "rewinddir" "rindex" "rmdir" "seek"
	      ;; "seekdir" "select" "semctl" "semget" "semop" "send"
	      ;; "setgrent" "sethostent" "setnetent" "setpgrp"
	      ;; "setpriority" "setprotoent" "setpwent" "setservent"
	      ;; "setsockopt" "shmctl" "shmget" "shmread" "shmwrite"
	      ;; "shutdown" "sin" "sleep" "socket" "socketpair"
	      ;; "sprintf" "sqrt" "srand" "stat" "substr" "symlink"
	      ;; "syscall" "sysopen" "sysread" "system" "syswrite" "tell"
	      ;; "telldir" "time" "times" "truncate" "uc" "ucfirst"
	      ;; "umask" "unlink" "unpack" "utime" "values" "vec"
	      ;; "wait" "waitpid" "wantarray" "warn" "write" "x" "xor"
	      "a\\(bs\\|ccept\\|tan2\\|larm\\|nd\\)\\|"
	      "b\\(in\\(d\\|mode\\)\\|less\\)\\|"
	      "c\\(h\\(r\\(\\|oot\\)\\|dir\\|mod\\|own\\)\\|aller\\|rypt\\|"
	      "lose\\(\\|dir\\)\\|mp\\|o\\(s\\|n\\(tinue\\|nect\\)\\)\\)\\|"
	      "CORE\\|d\\(ie\\|bm\\(close\\|open\\)\\|ump\\)\\|"
	      "e\\(x\\(p\\|it\\|ec\\)\\|q\\|nd\\(p\\(rotoent\\|went\\)\\|"
	      "hostent\\|servent\\|netent\\|grent\\)\\|of\\)\\|"
	      "f\\(ileno\\|cntl\\|lock\\|or\\(k\\|mline\\)\\)\\|"
	      "g\\(t\\|lob\\|mtime\\|e\\(\\|t\\(p\\(pid\\|r\\(iority\\|"
	      "oto\\(byn\\(ame\\|umber\\)\\|ent\\)\\)\\|eername\\|w"
	      "\\(uid\\|ent\\|nam\\)\\|grp\\)\\|host\\(by\\(addr\\|name\\)\\|"
	      "ent\\)\\|s\\(erv\\(by\\(port\\|name\\)\\|ent\\)\\|"
	      "ock\\(name\\|opt\\)\\)\\|c\\|login\\|net\\(by\\(addr\\|name\\)\\|"
	      "ent\\)\\|gr\\(ent\\|nam\\|gid\\)\\)\\)\\)\\|"
	      "hex\\|i\\(n\\(t\\|dex\\)\\|octl\\)\\|join\\|kill\\|"
	      "l\\(i\\(sten\\|nk\\)\\|stat\\|c\\(\\|first\\)\\|t\\|e"
	      "\\(\\|ngth\\)\\|o\\(c\\(altime\\|k\\)\\|g\\)\\)\\|m\\(sg\\(rcv\\|snd\\|"
	      "ctl\\|get\\)\\|kdir\\)\\|n\\(e\\|ot\\)\\|o\\(pen\\(\\|dir\\)\\|"
	      "r\\(\\|d\\)\\|ct\\)\\|p\\(ipe\\|ack\\)\\|quotemeta\\|"
	      "r\\(index\\|and\\|mdir\\|e\\(quire\\|ad\\(pipe\\|\\|lin"
	      "\\(k\\|e\\)\\|dir\\)\\|set\\|cv\\|verse\\|f\\|winddir\\|name"
	      "\\)\\)\\|s\\(printf\\|qrt\\|rand\\|tat\\|ubstr\\|e\\(t\\(p\\(r"
	      "\\(iority\\|otoent\\)\\|went\\|grp\\)\\|hostent\\|s\\(ervent\\|"
	      "ockopt\\)\\|netent\\|grent\\)\\|ek\\(\\|dir\\)\\|lect\\|"
	      "m\\(ctl\\|op\\|get\\)\\|nd\\)\\|h\\(utdown\\|m\\(read\\|ctl\\|"
	      "write\\|get\\)\\)\\|y\\(s\\(read\\|call\\|open\\|tem\\|write\\)\\|"
	      "mlink\\)\\|in\\|leep\\|ocket\\(pair\\|\\)\\)\\|t\\(runcate\\|"
	      "ell\\(\\|dir\\)\\|ime\\(\\|s\\)\\)\\|u\\(c\\(\\|first\\)\\|"
	      "time\\|mask\\|n\\(pack\\|link\\)\\)\\|v\\(alues\\|ec\\)\\|"
	      "w\\(a\\(rn\\|it\\(pid\\|\\)\\|ntarray\\)\\|rite\\)\\|"
	      "x\\(\\|or\\)\\|__\\(FILE__\\|LINE__\\|PACKAGE__\\)"
	      "\\)\\>") 2 'font-lock-type-face)
	    ;; In what follows we use `other' style
	    ;; for nonoverwritable builtins
	    ;; Somehow 's', 'm' are not auto-generated???
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      ;; "AUTOLOAD" "BEGIN" "CHECK" "DESTROY" "END" "INIT" "__END__" "chomp"
	      ;; "chop" "defined" "delete" "do" "each" "else" "elsif"
	      ;; "eval" "exists" "for" "foreach" "format" "goto"
	      ;; "grep" "if" "keys" "last" "local" "map" "my" "next"
	      ;; "no" "package" "pop" "pos" "print" "printf" "push"
	      ;; "q" "qq" "qw" "qx" "redo" "return" "scalar" "shift"
	      ;; "sort" "splice" "split" "study" "sub" "tie" "tr"
	      ;; "undef" "unless" "unshift" "untie" "until" "use"
	      ;; "while" "y"
	      "AUTOLOAD\\|BEGIN\\|CHECK\\|cho\\(p\\|mp\\)\\|d\\(e\\(fined\\|lete\\)\\|"
	      "o\\)\\|DESTROY\\|e\\(ach\\|val\\|xists\\|ls\\(e\\|if\\)\\)\\|"
	      "END\\|for\\(\\|each\\|mat\\)\\|g\\(rep\\|oto\\)\\|INIT\\|if\\|keys\\|"
	      "l\\(ast\\|ocal\\)\\|m\\(ap\\|y\\)\\|n\\(ext\\|o\\)\\|our\\|"
	      "p\\(ackage\\|rint\\(\\|f\\)\\|ush\\|o\\(p\\|s\\)\\)\\|"
	      "q\\(\\|q\\|w\\|x\\|r\\)\\|re\\(turn\\|do\\)\\|s\\(pli\\(ce\\|t\\)\\|"
	      "calar\\|tudy\\|ub\\|hift\\|ort\\)\\|t\\(r\\|ie\\)\\|"
	      "u\\(se\\|n\\(shift\\|ti\\(l\\|e\\)\\|def\\|less\\)\\)\\|"
	      "while\\|y\\|__\\(END\\|DATA\\)__" ;__DATA__ added manually
	      "\\|[sm]"			; Added manually
	      "\\)\\>") 2 'cperl-nonoverridable-face)
	    ;;		(mapconcat 'identity
	    ;;			   '("#endif" "#else" "#ifdef" "#ifndef" "#if"
	    ;;			     "#include" "#define" "#undef")
	    ;;			   "\\|")
	    '("-[rwxoRWXOezsfdlpSbctugkTBMAC]\\>\\([ \t]+_\\>\\)?" 0
	      font-lock-function-name-face keep) ; Not very good, triggers at "[a-z]"
	    '("\\<sub[ \t]+\\([^ \t{;()]+\\)[ \t]*\\(([^()]*)[ \t]*\\)?[#{\n]" 1
	      font-lock-function-name-face)
	    '("\\<\\(package\\|require\\|use\\|import\\|no\\|bootstrap\\)[ \t]+\\([a-zA-z_][a-zA-z_0-9:]*\\)[ \t;]" ; require A if B;
	      2 font-lock-function-name-face)
	    '("^[ \t]*format[ \t]+\\([a-zA-z_][a-zA-z_0-9:]*\\)[ \t]*=[ \t]*$"
	      1 font-lock-function-name-face)
	    (cond ((featurep 'font-lock-extra)
		   '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		     (2 font-lock-string-face t)
		     (0 '(restart 2 t)))) ; To highlight $a{bc}{ef}
		  (font-lock-anchored
		   '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		     (2 font-lock-string-face t)
		     ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		      nil nil
		      (1 font-lock-string-face t))))
		  (t '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		       2 font-lock-string-face t)))
	    '("[\[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1
	      font-lock-string-face t)
	    '("^[ \t]*\\([a-zA-Z0-9_]+[ \t]*:\\)[ \t]*\\($\\|{\\|\\<\\(until\\|while\\|for\\(each\\)?\\|do\\)\\>\\)" 1
	      font-lock-constant-face) ; labels
	    '("\\<\\(continue\\|next\\|last\\|redo\\|goto\\)\\>[ \t]+\\([a-zA-Z0-9_:]+\\)" ; labels as targets
	      2 font-lock-constant-face)
	    ;; Uncomment to get perl-mode-like vars
            ;;; '("[$*]{?\\(\\sw+\\)" 1 font-lock-variable-name-face)
            ;;; '("\\([@%]\\|\\$#\\)\\(\\sw+\\)"
            ;;;  (2 (cons font-lock-variable-name-face '(underline))))
	    (cond ((featurep 'font-lock-extra)
		   '("^[ \t]*\\(my\\|local\\|our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)\\([ \t]*,\\)?"
		     (3 font-lock-variable-name-face)
		     (4 '(another 4 nil
				  ("\\=[ \t]*,[ \t]*\\([$@%*][a-zA-Z0-9_:]+\\)\\([ \t]*,\\)?"
				   (1 font-lock-variable-name-face)
				   (2 '(restart 2 nil) nil t)))
			nil t)))	; local variables, multiple
		  (font-lock-anchored
		   '("^[ \t{}]*\\(my\\|local\\|our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)"
		     (3 font-lock-variable-name-face)
		     ("\\=[ \t]*,[ \t]*\\([$@%*][a-zA-Z0-9_:]+\\)"
		      nil nil
		      (1 font-lock-variable-name-face))))
		  (t '("^[ \t{}]*\\(my\\|local\\our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)"
		       3 font-lock-variable-name-face)))
	    '("\\<for\\(each\\)?\\([ \t]+\\(my\\|local\\|our\\)\\)?[ \t]*\\(\\$[a-zA-Z_][a-zA-Z_0-9]*\\)[ \t]*("
	      4 font-lock-variable-name-face)))
	  (setq 
	   t-font-lock-keywords-1
	   (and (fboundp 'turn-on-font-lock) ; Check for newer font-lock
		(not cperl-xemacs-p) ; not yet as of XEmacs 19.12
		'(
		  ("\\(\\([@%]\\|\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
		   (if (eq (char-after (match-beginning 2)) ?%)
		       cperl-hash-face
		     cperl-array-face)
		   t)			; arrays and hashes
		  ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)"
		   1
		   (if (= (- (match-end 2) (match-beginning 2)) 1)
		       (if (eq (char-after (match-beginning 3)) ?{)
			   cperl-hash-face
			 cperl-array-face) ; arrays and hashes
		     font-lock-variable-name-face) ; Just to put something
		   t)
		  ;;("\\([smy]\\|tr\\)\\([^a-z_A-Z0-9]\\)\\(\\([^\n\\]*||\\)\\)\\2")
		       ;;; Too much noise from \s* @s[ and friends
		  ;;("\\(\\<\\([msy]\\|tr\\)[ \t]*\\([^ \t\na-zA-Z0-9_]\\)\\|\\(/\\)\\)"
		  ;;(3 font-lock-function-name-face t t)
		  ;;(4
		  ;; (if (cperl-slash-is-regexp)
		  ;;    font-lock-function-name-face 'default) nil t))
		  )))
	  (if cperl-highlight-variables-indiscriminately
	      (setq t-font-lock-keywords-1
		    (append t-font-lock-keywords-1
			    (list '("[$*]{?\\(\\sw+\\)" 1
				    font-lock-variable-name-face)))))
	  (setq cperl-font-lock-keywords-1 
		(if cperl-syntaxify-by-font-lock
		    (cons 'cperl-fontify-update
			  t-font-lock-keywords)
		  t-font-lock-keywords)
		cperl-font-lock-keywords cperl-font-lock-keywords-1
		cperl-font-lock-keywords-2 (append
					   cperl-font-lock-keywords-1
					   t-font-lock-keywords-1)))
	(if (fboundp 'ps-print-buffer) (cperl-ps-print-init))
	(if (or (featurep 'choose-color) (featurep 'font-lock-extra))
	    (eval			; Avoid a warning
	    '(font-lock-require-faces
	     (list
	      ;; Color-light    Color-dark      Gray-light      Gray-dark Mono
	      (list 'font-lock-comment-face
		    ["Firebrick"	"OrangeRed" 	"DimGray"	"Gray80"]
		    nil
		    [nil		nil		t		t	t]
		    [nil		nil		t		t	t]
		    nil)
	      (list 'font-lock-string-face
		    ["RosyBrown"	"LightSalmon" 	"Gray50"	"LightGray"]
		    nil
		    nil
		    [nil		nil		t		t	t]
		    nil)
	      (list 'font-lock-function-name-face
		    (vector
		     "Blue"		"LightSkyBlue"	"Gray50"	"LightGray"
		     (cdr (assq 'background-color ; if mono
				(frame-parameters))))
		    (vector
		     nil		nil		nil		nil
		     (cdr (assq 'foreground-color ; if mono
				(frame-parameters))))
		    [nil		nil		t		t	t]
		    nil
		    nil)
	      (list 'font-lock-variable-name-face
		    ["DarkGoldenrod"	"LightGoldenrod" "DimGray"	"Gray90"]
		    nil
		    [nil		nil		t		t	t]
		    [nil		nil		t		t	t]
		    nil)
	      (list 'font-lock-type-face
		    ["DarkOliveGreen"	"PaleGreen" 	"DimGray"	"Gray80"]
		    nil
		    [nil		nil		t		t	t]
		    nil
		    [nil		nil		t		t	t]
		    )
	      (list 'font-lock-constant-face
		    ["CadetBlue"	"Aquamarine" 	"Gray50"	"LightGray"]
		    nil
		    [nil		nil		t		t	t]
		    nil
		    [nil		nil		t		t	t]
		    )
	      (list 'cperl-nonoverridable-face
		    ["chartreuse3"	("orchid1" "orange")
		     nil		"Gray80"]
		    [nil		nil		"gray90"]
		    [nil		nil		nil		t	t]
		    [nil		nil		t		t]
		    [nil		nil		t		t	t]
		    )
	      (list 'cperl-array-face
		    ["blue"		"yellow" 	nil		"Gray80"]
		    ["lightyellow2"	("navy" "os2blue" "darkgreen")
		     "gray90"]
		    t
		    nil
		    nil)
	      (list 'cperl-hash-face
		    ["red"		"red"	 	nil		"Gray80"]
		    ["lightyellow2"	("navy" "os2blue" "darkgreen")
		     "gray90"]
		    t
		    t
		    nil))))
	  ;; Do it the dull way, without choose-color
	  (defvar cperl-guessed-background nil
	    "Display characteristics as guessed by cperl.")
;;	  (or (fboundp 'x-color-defined-p)
;;	      (defalias 'x-color-defined-p
;;		(cond ((fboundp 'color-defined-p) 'color-defined-p)
;;		      ;; XEmacs >= 19.12
;;		      ((fboundp 'valid-color-name-p) 'valid-color-name-p)
;;		      ;; XEmacs 19.11
;;		      (t 'x-valid-color-name-p))))
	  (cperl-force-face font-lock-constant-face
			    "Face for constant and label names")
	  (cperl-force-face font-lock-variable-name-face
			    "Face for variable names")
	  (cperl-force-face font-lock-type-face
			    "Face for data types")
	  (cperl-force-face cperl-nonoverridable-face
			    "Face for data types from another group")
	  (cperl-force-face font-lock-comment-face
			    "Face for comments")
	  (cperl-force-face font-lock-function-name-face
			    "Face for function names")
	  (cperl-force-face cperl-hash-face
			    "Face for hashes")
	  (cperl-force-face cperl-array-face
			    "Face for arrays")
	  ;;(defvar font-lock-constant-face 'font-lock-constant-face)
	  ;;(defvar font-lock-variable-name-face 'font-lock-variable-name-face)
	  ;;(or (boundp 'font-lock-type-face)
	  ;;    (defconst font-lock-type-face
	  ;;	'font-lock-type-face
	  ;;	"Face to use for data types."))
	  ;;(or (boundp 'cperl-nonoverridable-face)
	  ;;    (defconst cperl-nonoverridable-face
	  ;;	'cperl-nonoverridable-face
	  ;;	"Face to use for data types from another group."))
	  ;;(if (not cperl-xemacs-p) nil
	  ;;  (or (boundp 'font-lock-comment-face)
	  ;;	(defconst font-lock-comment-face
	  ;;	  'font-lock-comment-face
	  ;;	  "Face to use for comments."))
	  ;;  (or (boundp 'font-lock-keyword-face)
	  ;;	(defconst font-lock-keyword-face
	  ;;	  'font-lock-keyword-face
	  ;;	  "Face to use for keywords."))
	  ;;  (or (boundp 'font-lock-function-name-face)
	  ;;	(defconst font-lock-function-name-face
	  ;;	  'font-lock-function-name-face
	  ;;	  "Face to use for function names.")))
	  (if (and
	       (not (cperl-is-face 'cperl-array-face))
	       (cperl-is-face 'font-lock-emphasized-face))
	      (copy-face 'font-lock-emphasized-face 'cperl-array-face))
	  (if (and
	       (not (cperl-is-face 'cperl-hash-face))
	       (cperl-is-face 'font-lock-other-emphasized-face))
	      (copy-face 'font-lock-other-emphasized-face
			 'cperl-hash-face))
	  (if (and
	       (not (cperl-is-face 'cperl-nonoverridable-face))
	       (cperl-is-face 'font-lock-other-type-face))
	      (copy-face 'font-lock-other-type-face
			 'cperl-nonoverridable-face))
	  ;;(or (boundp 'cperl-hash-face)
	  ;;    (defconst cperl-hash-face
	  ;;	'cperl-hash-face
	  ;;	"Face to use for hashes."))
	  ;;(or (boundp 'cperl-array-face)
	  ;;    (defconst cperl-array-face
	  ;;	'cperl-array-face
	  ;;	"Face to use for arrays."))
	  ;; Here we try to guess background
	  (let ((background
		 (if (boundp 'font-lock-background-mode)
		     font-lock-background-mode
		   'light))
		(face-list (and (fboundp 'face-list) (face-list)))
		;; cperl-is-face
		)
;;;;	    (fset 'cperl-is-face
;;;;		  (cond ((fboundp 'find-face)
;;;;			 (symbol-function 'find-face))
;;;;			(face-list
;;;;			 (function (lambda (face) (member face face-list))))
;;;;			(t
;;;;			 (function (lambda (face) (boundp face))))))
	    (defvar cperl-guessed-background
	      (if (and (boundp 'font-lock-display-type)
		       (eq font-lock-display-type 'grayscale))
		  'gray
		background)
	      "Background as guessed by CPerl mode")
	    (if (and
		 (not (cperl-is-face 'font-lock-constant-face))
		 (cperl-is-face 'font-lock-reference-face))
	      (copy-face 'font-lock-reference-face 'font-lock-constant-face))
	    (if (cperl-is-face 'font-lock-type-face) nil
	      (copy-face 'default 'font-lock-type-face)
	      (cond
	       ((eq background 'light)
		(set-face-foreground 'font-lock-type-face
				     (if (x-color-defined-p "seagreen")
					 "seagreen"
				       "sea green")))
	       ((eq background 'dark)
		(set-face-foreground 'font-lock-type-face
				     (if (x-color-defined-p "os2pink")
					 "os2pink"
				       "pink")))
	       (t
		(set-face-background 'font-lock-type-face "gray90"))))
	    (if (cperl-is-face 'cperl-nonoverridable-face)
		nil
	      (copy-face 'font-lock-type-face 'cperl-nonoverridable-face)
	      (cond
	       ((eq background 'light)
		(set-face-foreground 'cperl-nonoverridable-face
				     (if (x-color-defined-p "chartreuse3")
					 "chartreuse3"
				       "chartreuse")))
	       ((eq background 'dark)
		(set-face-foreground 'cperl-nonoverridable-face
				     (if (x-color-defined-p "orchid1")
					 "orchid1"
				       "orange")))))
;;;	    (if (cperl-is-face 'font-lock-other-emphasized-face) nil
;;;	      (copy-face 'bold-italic 'font-lock-other-emphasized-face)
;;;	      (cond
;;;	       ((eq background 'light)
;;;		(set-face-background 'font-lock-other-emphasized-face
;;;				     (if (x-color-defined-p "lightyellow2")
;;;					 "lightyellow2"
;;;				       (if (x-color-defined-p "lightyellow")
;;;					   "lightyellow"
;;;					 "light yellow"))))
;;;	       ((eq background 'dark)
;;;		(set-face-background 'font-lock-other-emphasized-face
;;;				     (if (x-color-defined-p "navy")
;;;					 "navy"
;;;				       (if (x-color-defined-p "darkgreen")
;;;					   "darkgreen"
;;;					 "dark green"))))
;;;	       (t (set-face-background 'font-lock-other-emphasized-face "gray90"))))
;;;	    (if (cperl-is-face 'font-lock-emphasized-face) nil
;;;	      (copy-face 'bold 'font-lock-emphasized-face)
;;;	      (cond
;;;	       ((eq background 'light)
;;;		(set-face-background 'font-lock-emphasized-face
;;;				     (if (x-color-defined-p "lightyellow2")
;;;					 "lightyellow2"
;;;				       "lightyellow")))
;;;	       ((eq background 'dark)
;;;		(set-face-background 'font-lock-emphasized-face
;;;				     (if (x-color-defined-p "navy")
;;;					 "navy"
;;;				       (if (x-color-defined-p "darkgreen")
;;;					   "darkgreen"
;;;					 "dark green"))))
;;;	       (t (set-face-background 'font-lock-emphasized-face "gray90"))))
	    (if (cperl-is-face 'font-lock-variable-name-face) nil
	      (copy-face 'italic 'font-lock-variable-name-face))
	    (if (cperl-is-face 'font-lock-constant-face) nil
	      (copy-face 'italic 'font-lock-constant-face))))
	(setq cperl-faces-init t))
    (error (message "cperl-init-faces (ignored): %s" errs))))


(defun cperl-ps-print-init ()
  "Initialization of `ps-print' components for faces used in CPerl."
  (eval-after-load "ps-print"
    '(setq ps-bold-faces
	   ;; 			font-lock-variable-name-face
	   ;;			font-lock-constant-face
	   (append '(cperl-array-face
		     cperl-hash-face)
		   ps-bold-faces)
	   ps-italic-faces
	   ;;			font-lock-constant-face
	   (append '(cperl-nonoverridable-face
		     cperl-hash-face)
		   ps-italic-faces)
	   ps-underlined-faces
	   ;;	     font-lock-type-face
	   (append '(cperl-array-face
		     cperl-hash-face
		     underline
		     cperl-nonoverridable-face)
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
    (cperl-ps-extend-face-list cperl-ps-print-face-properties)
    (ps-print-buffer-with-faces file)))

;;; (defun cperl-ps-print-init ()
;;;   "Initialization of `ps-print' components for faces used in CPerl."
;;;   ;; Guard against old versions
;;;   (defvar ps-underlined-faces nil)
;;;   (defvar ps-bold-faces nil)
;;;   (defvar ps-italic-faces nil)
;;;   (setq ps-bold-faces
;;; 	(append '(font-lock-emphasized-face
;;; 		  cperl-array-face
;;; 		  font-lock-keyword-face
;;; 		  font-lock-variable-name-face
;;; 		  font-lock-constant-face
;;; 		  font-lock-reference-face
;;; 		  font-lock-other-emphasized-face
;;; 		  cperl-hash-face)
;;; 		ps-bold-faces))
;;;   (setq ps-italic-faces
;;; 	(append '(cperl-nonoverridable-face
;;; 		  font-lock-constant-face
;;; 		  font-lock-reference-face
;;; 		  font-lock-other-emphasized-face
;;; 		  cperl-hash-face)
;;; 		ps-italic-faces))
;;;   (setq ps-underlined-faces
;;; 	(append '(font-lock-emphasized-face
;;; 		  cperl-array-face
;;; 		  font-lock-other-emphasized-face
;;; 		  cperl-hash-face
;;; 		  cperl-nonoverridable-face font-lock-type-face)
;;; 		ps-underlined-faces))
;;;   (cons 'font-lock-type-face ps-underlined-faces))


(if (cperl-enable-font-lock) (cperl-windowed-init))

(defconst cperl-styles-entries
  '(cperl-indent-level cperl-brace-offset cperl-continued-brace-offset
    cperl-label-offset cperl-extra-newline-before-brace
    cperl-merge-trailing-else
    cperl-continued-statement-offset))

(defconst cperl-style-alist
  '(("CPerl" ; =GNU without extra-newline-before-brace
     (cperl-indent-level               .  2)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-merge-trailing-else	       .  t)
     (cperl-continued-statement-offset .  2))
    ("PerlStyle" ; CPerl with 4 as indent
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -4)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-merge-trailing-else	       .  t)
     (cperl-continued-statement-offset .  4))
    ("GNU"
     (cperl-indent-level               .  2)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-extra-newline-before-brace .  t)
     (cperl-merge-trailing-else	       .  nil)
     (cperl-continued-statement-offset .  2))
    ("K&R"
     (cperl-indent-level               .  5)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -5)
     (cperl-label-offset               . -5)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     (cperl-merge-trailing-else	       .  nil)
     (cperl-continued-statement-offset .  5))
    ("BSD"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -4)
     (cperl-label-offset               . -4)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     (cperl-continued-statement-offset .  4))
    ("C++"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -4)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     (cperl-merge-trailing-else	       .  nil)
     (cperl-extra-newline-before-brace .  t))
    ("Current")
    ("Whitesmith"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -4)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     (cperl-continued-statement-offset .  4)))
  "(Experimental) list of variables to set to get a particular indentation style.
Should be used via `cperl-set-style' or via Perl menu.")

(defun cperl-set-style (style)
  "Set CPerl-mode variables to use one of several different indentation styles.
The arguments are a string representing the desired style.
The list of styles is in `cperl-style-alist', available styles
are GNU, K&R, BSD, C++ and Whitesmith.

The current value of style is memorized (unless there is a memorized
data already), may be restored by `cperl-set-style-back'.

Chosing \"Current\" style will not change style, so this may be used for
side-effect of memorizing only."
  (interactive
   (let ((list (mapcar (function (lambda (elt) (list (car elt))))
		       cperl-style-alist)))
     (list (completing-read "Enter style: " list nil 'insist))))
  (or cperl-old-style
      (setq cperl-old-style
	    (mapcar (function
		     (lambda (name)
		       (cons name (eval name))))
		    cperl-styles-entries)))
  (let ((style (cdr (assoc style cperl-style-alist))) setting str sym)
    (while style
      (setq setting (car style) style (cdr style))
      (set (car setting) (cdr setting)))))

(defun cperl-set-style-back ()
  "Restore a style memorised by `cperl-set-style'."
  (interactive)
  (or cperl-old-style (error "The style was not changed"))
  (let (setting)
    (while cperl-old-style
      (setq setting (car cperl-old-style)
	    cperl-old-style (cdr cperl-old-style))
      (set (car setting) (cdr setting)))))

(defun cperl-check-syntax ()
  (interactive)
  (require 'mode-compile)
  (let ((perl-dbg-flags (concat cperl-extra-perl-args " -wc")))
    (eval '(mode-compile))))		; Avoid a warning

(defun cperl-info-buffer (type)
  ;; Returns buffer with documentation.  Creates if missing.
  ;; If TYPE, this vars buffer.
  ;; Special care is taken to not stomp over an existing info buffer
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
	(make-local-variable 'window-min-height)
	(setq window-min-height 2)
	(current-buffer)))))

(defun cperl-word-at-point (&optional p)
  ;; Returns the word at point or at P.
  (save-excursion
    (if p (goto-char p))
    (or (cperl-word-at-point-hard)
	(progn
	  (require 'etags)
	  (funcall (or (and (boundp 'find-tag-default-function)
			    find-tag-default-function)
		       (get major-mode 'find-tag-default-function)
		       ;; XEmacs 19.12 has `find-tag-default-hook'; it is
		       ;; automatically used within `find-tag-default':
		       'find-tag-default))))))

(defun cperl-info-on-command (command)
  "Show documentation for Perl command in other window.
If perl-info buffer is shown in some frame, uses this frame.
Customized by setting variables `cperl-shrink-wrap-info-frame',
`cperl-max-help-size'."
  (interactive
   (let* ((default (cperl-word-at-point))
	  (read (read-string
		     (format "Find doc for Perl function (default %s): "
			     default))))
     (list (if (equal read "")
		   default
		 read))))

  (let ((buffer (current-buffer))
	(cmd-desc (concat "^" (regexp-quote command) "[^a-zA-Z_0-9]")) ; "tr///"
	pos isvar height iniheight frheight buf win fr1 fr2 iniwin not-loner
	max-height char-height buf-list)
    (if (string-match "^-[a-zA-Z]$" command)
	(setq cmd-desc "^-X[ \t\n]"))
    (setq isvar (string-match "^[$@%]" command)
	  buf (cperl-info-buffer isvar)
	  iniwin (selected-window)
	  fr1 (window-frame iniwin))
    (set-buffer buf)
    (beginning-of-buffer)
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
	  (and win (setq fr2 (window-frame win)))
	  (if (or (not fr2) (eq fr1 fr2))
	      (pop-to-buffer buf)
	    (special-display-popup-frame buf) ; Make it visible
	    (select-window win))
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
			 ;; Non-functioning under OS/2:
			 (if (eq char-height 1) (setq char-height 18))
			 ;; Title, menubar, + 2 for slack
			 (- (/ (x-display-pixel-height) char-height) 4)
			 ))
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
  "Show documentation for Perl command at point in other window."
  (interactive)
  (cperl-info-on-command (cperl-word-at-point)))

(defun cperl-imenu-info-imenu-search ()
  (if (looking-at "^-X[ \t\n]") nil
    (re-search-backward
     "^\n\\([-a-zA-Z_]+\\)[ \t\n]")
    (forward-line 1)))

(defun cperl-imenu-info-imenu-name ()
  (buffer-substring
   (match-beginning 1) (match-end 1)))

(defun cperl-imenu-on-info ()
  (interactive)
  (let* ((buffer (current-buffer))
	 imenu-create-index-function
	 imenu-prev-index-position-function
	 imenu-extract-index-name-function
	 (index-item (save-restriction
		       (save-window-excursion
			 (set-buffer (cperl-info-buffer nil))
			 (setq imenu-create-index-function
			       'imenu-default-create-index-function
			       imenu-prev-index-position-function
			       'cperl-imenu-info-imenu-search
			       imenu-extract-index-name-function
			       'cperl-imenu-info-imenu-name)
			 (imenu-choose-buffer-index)))))
    (and index-item
	 (progn
	   (push-mark)
	   (pop-to-buffer "*info-perl*")
	   (cond
	    ((markerp (cdr index-item))
	     (goto-char (marker-position (cdr index-item))))
	    (t
	     (goto-char (cdr index-item))))
	   (set-window-start (selected-window) (point))
	   (pop-to-buffer buffer)))))

(defun cperl-lineup (beg end &optional step minshift)
  "Lineup construction in a region.
Beginning of region should be at the start of a construction.
All first occurrences of this construction in the lines that are
partially contained in the region are lined up at the same column.

MINSHIFT is the minimal amount of space to insert before the construction.
STEP is the tabwidth to position constructions.
If STEP is nil, `cperl-lineup-step' will be used
\(or `cperl-indent-level', if `cperl-lineup-step' is `nil').
Will not move the position at the start to the left."
  (interactive "r")
  (let (search col tcol seen b e)
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
	    (setq e (point))
	    (skip-chars-backward " \t")
	    (delete-region (point) e)
	    (indent-to-column col); (make-string (- col (current-column)) ?\ ))
	    (beginning-of-line 2)
	    (and (< (point) end)
		 (re-search-forward search end t)
		 (goto-char (match-beginning 0)))))))) ; No body

(defun cperl-etags (&optional add all files)
  "Run etags with appropriate options for Perl files.
If optional argument ALL is `recursive', will process Perl files
in subdirectories too."
  (interactive)
  (let ((cmd "etags")
	(args '("-l" "none" "-r" "/\\<\\(package\\|sub\\)[ \\t]+\\(\\([a-zA-Z0-9:_]*::\\)?\\([a-zA-Z0-9_]+\\)[ \\t]*\\(([^()]*)[ \t]*\\)?\\([{#]\\|$\\)\\)/\\4/"))
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
  "Toggle the state of automatic help message in CPerl mode.
See `cperl-lazy-help-time' too."
  (interactive)
  (if (fboundp 'run-with-idle-timer)
      (progn
	(if cperl-lazy-installed
	    (eval '(cperl-lazy-unstall))
	  (cperl-lazy-install))
	(message "Perl help messages will %sbe automatically shown now."
		 (if cperl-lazy-installed "" "not ")))
    (message "Cannot automatically show Perl help messages - run-with-idle-timer missing.")))

(defun cperl-toggle-construct-fix ()
  "Toggle whether `indent-region'/`indent-sexp' fix whitespace too."
  (interactive)
  (setq cperl-indent-region-fix-constructs
	(if cperl-indent-region-fix-constructs
	    nil
	  1))
  (message "indent-region/indent-sexp will %sbe automatically fix whitespace."
	   (if cperl-indent-region-fix-constructs "" "not ")))

;;;; Tags file creation.

(defvar cperl-tmp-buffer " *cperl-tmp*")

(defun cperl-setup-tmp-buf ()
  (set-buffer (get-buffer-create cperl-tmp-buffer))
  (set-syntax-table cperl-mode-syntax-table)
  (buffer-disable-undo)
  (auto-fill-mode 0)
  (if cperl-use-syntax-table-text-property-for-tags
      (progn
	(make-local-variable 'parse-sexp-lookup-properties)
	;; Do not introduce variable if not needed, we check it!
	(set 'parse-sexp-lookup-properties t))))

(defun cperl-xsub-scan ()
  (require 'imenu)
  (let ((index-alist '())
	(prev-pos 0) index index1 name package prefix)
    (goto-char (point-min))
    (if noninteractive
	(message "Scanning XSUB for index")
      (imenu-progress-message prev-pos 0))
    ;; Search for the function
    (progn ;;save-match-data
      (while (re-search-forward
	      "^\\([ \t]*MODULE\\>[^\n]*\\<PACKAGE[ \t]*=[ \t]*\\([a-zA-Z_][a-zA-Z_0-9:]*\\)\\>\\|\\([a-zA-Z_][a-zA-Z_0-9]*\\)(\\|[ \t]*BOOT:\\)"
	      nil t)
	(or noninteractive
	    (imenu-progress-message prev-pos))
	(cond
	 ((match-beginning 2)	; SECTION
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
	  (setq index (imenu-example--name-and-position))
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
	  (setq index (imenu-example--name-and-position))
	  (setcar index (concat package "::BOOT:"))
	  (push index index-alist)))))
    (or noninteractive
	(imenu-progress-message prev-pos 100))
    index-alist))

(defvar cperl-unreadable-ok nil)

(defun cperl-find-tags (ifile xs topdir)
  (let (ind (b (get-buffer cperl-tmp-buffer)) lst elt pos ret rel
	    (cperl-pod-here-fontify nil) f file)
    (save-excursion
      (if b (set-buffer b)
	  (cperl-setup-tmp-buf))
      (erase-buffer)
      (condition-case err
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
	    (error (message "While scanning for syntax: %s" err))))
      (if xs
	  (setq lst (cperl-xsub-scan))
	(setq ind (cperl-imenu--create-perl-index))
	(setq lst (cdr (assoc "+Unsorted List+..." ind))))
      (setq lst
	    (mapcar
	     (function
	      (lambda (elt)
		(cond ((string-match "^[_a-zA-Z]" (car elt))
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
						 (point))))))))
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
			(car elt) )
		      1
		      (number-to-string (elt elt 2)) ; Line
		      ","
		      (number-to-string (1- (elt elt 1))) ; Char pos 0-based
		      "\n")
	      (if (and (string-match "^[_a-zA-Z]+::" (car elt))
		       (string-match "^sub[ \t]+\\([_a-zA-Z]+\\)[^:_a-zA-Z]"
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
      ;; On case-preserving filesystems (EMX on OS/2) case might be encoded in properties
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
  "Add to TAGS data for Perl and XSUB files in the current directory and kids.
Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \
        -f cperl-add-tags-recurse
"
  (cperl-write-tags nil nil t t nil t))

(defun cperl-add-tags-recurse ()
  "Add to TAGS file data for Perl files in the current directory and kids.
Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \
        -f cperl-add-tags-recurse
"
  (cperl-write-tags nil nil t t))

(defun cperl-write-tags (&optional file erase recurse dir inbuffer noxs topdir)
  ;; If INBUFFER, do not select buffer, and do not save
  ;; If ERASE is `ignore', do not erase, and do not try to delete old info.
  (require 'etags)
  (if file nil
    (setq file (if dir default-directory (buffer-file-name)))
    (if (and (not dir) (buffer-modified-p)) (error "Save buffer first!")))
  (or topdir
      (setq topdir default-directory))
  (let ((tags-file-name "TAGS")
	(case-fold-search (eq system-type 'emx))
	xs rel tm)
    (save-excursion
      (cond (inbuffer nil)		; Already there
	    ((file-exists-p tags-file-name)
	     (if cperl-xemacs-p
		 (visit-tags-table-buffer)
	      (visit-tags-table-buffer tags-file-name)))
	    (t (set-buffer (find-file-noselect tags-file-name))))
      (cond
       (dir
	(cond ((eq erase 'ignore))
	      (erase
	       (erase-buffer)
	       (setq erase 'ignore)))
	(let ((files 
	       (condition-case err
		   (directory-files file t 
				    (if recurse nil cperl-scan-files-regexp)
				    t)
		 (error
		  (if cperl-unreadable-ok nil
		    (if (y-or-n-p
			 (format "Directory %s unreadable.  Continue? " file))
			(setq cperl-unreadable-ok t 
			      tm nil) ; Return empty list
		      (error "Aborting: unreadable directory %s" file)))))))
	  (mapcar (function (lambda (file)
			      (cond
			       ((string-match cperl-noscan-files-regexp file)
				nil)
			       ((not (file-directory-p file))
				(if (string-match cperl-scan-files-regexp file)
				    (cperl-write-tags file erase recurse nil t noxs topdir)))
			       ((not recurse) nil)
			       (t (cperl-write-tags file erase recurse t t noxs topdir)))))
		  files))
	)
       (t
	(setq xs (string-match "\\.xs$" file))
	(if (not (and xs noxs))
	    (progn
	      (cond ((eq erase 'ignore) (goto-char (point-max)))
		  (erase (erase-buffer))
		  (t
		   (goto-char 1)
		   (setq rel file)
		   ;; On case-preserving filesystems (EMX on OS/2) case might be encoded in properties
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
      (if inbuffer nil		; Delegate to the caller
	(save-buffer 0)		; No backup
	(if (fboundp 'initialize-new-tags-table) ; Do we need something special in XEmacs?
	    (initialize-new-tags-table))))))

(defvar cperl-tags-hier-regexp-list
  (concat
   "^\\("
      "\\(package\\)\\>"
     "\\|"
      "sub\\>[^\n]+::"
     "\\|"
      "[a-zA-Z_][a-zA-Z_0-9:]*(\C-?[^\n]+::" ; XSUB?
     "\\|"
      "[ \t]*BOOT:\C-?[^\n]+::"		; BOOT section
   "\\)"))

(defvar cperl-hierarchy '(() ())
  "Global hierarchy of classes")

(defun cperl-tags-hier-fill ()
  ;; Suppose we are in a tag table cooked by cperl.
  (goto-char 1)
  (let (type pack name pos line chunk ord cons1 file str info fileind)
    (while (re-search-forward cperl-tags-hier-regexp-list nil t)
      (setq pos (match-beginning 0)
	    pack (match-beginning 2))
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
		  info (cperl-etags-snarf-tag file line))
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

(defun cperl-tags-hier-init (&optional update)
  "Show hierarchical menu of classes and methods.
Finds info about classes by a scan of loaded TAGS files.
Supposes that the TAGS files contain fully qualified function names.
One may build such TAGS files from CPerl mode menu."
  (interactive)
  (require 'etags)
  (require 'imenu)
  (if (or update (null (nth 2 cperl-hierarchy)))
      (let (pack name cons1 to l1 l2 l3 l4 b
		 (remover (function (lambda (elt) ; (name (file1...) (file2..))
				      (or (nthcdr 2 elt)
					  ;; Only in one file
					  (setcdr elt (cdr (nth 1 elt))))))))
	;; (setq cperl-hierarchy '(() () ())) ; Would write into '() later!
	(setq cperl-hierarchy (list l1 l2 l3))
	(if cperl-xemacs-p		; Not checked
	    (progn
	      (or tags-file-name
		  ;; Does this work in XEmacs?
	    (call-interactively 'visit-tags-table))
	(message "Updating list of classes...")
	      (set-buffer (get-file-buffer tags-file-name))
	      (cperl-tags-hier-fill))
	  (or tags-table-list
	      (call-interactively 'visit-tags-table))
	(mapcar
	 (function
	  (lambda (tagsfile)
	      (message "Updating list of classes... %s" tagsfile)
	    (set-buffer (get-file-buffer tagsfile))
	    (cperl-tags-hier-fill)))
	 tags-table-list)
	  (message "Updating list of classes... postprocessing..."))
	(mapcar remover (car cperl-hierarchy))
	(mapcar remover (nth 1 cperl-hierarchy))
	(setq to (list nil (cons "Packages: " (nth 1 cperl-hierarchy))
		       (cons "Methods: " (car cperl-hierarchy))))
	(cperl-tags-treeify to 1)
	(setcar (nthcdr 2 cperl-hierarchy)
		(cperl-menu-to-keymap (cons '("+++UPDATE+++" . -999) (cdr to))))
	(message "Updating list of classes: done, requesting display...")
	;;(cperl-imenu-addback (nth 2 cperl-hierarchy))
	))
  (or (nth 2 cperl-hierarchy)
      (error "No items found"))
  (setq update
;;;	(imenu-choose-buffer-index "Packages: " (nth 2 cperl-hierarchy))
	(if window-system
	    (x-popup-menu t (nth 2 cperl-hierarchy))
	  (require 'tmm)
	  (tmm-prompt (nth 2 cperl-hierarchy))))
  (if (and update (listp update))
      (progn (while (cdr update) (setq update (cdr update)))
	     (setq update (car update)))) ; Get the last from the list
  (if (vectorp update)
      (progn
	(find-file (elt update 0))
	(cperl-etags-goto-tag-location (elt update 1))))
  (if (eq update -999) (cperl-tags-hier-init t)))

(defun cperl-tags-treeify (to level)
  ;; cadr of `to' is read-write.  On start it is a cons
  (let* ((regexp (concat "^\\(" (mapconcat
				 'identity
				 (make-list level "[_a-zA-Z0-9]+")
				 "::")
			 "\\)\\(::\\)?"))
	 (packages (cdr (nth 1 to)))
	 (methods (cdr (nth 2 to)))
	 l1 head tail cons1 cons2 ord writeto packs recurse
	 root-packages root-functions ms many_ms same_name ps
	 (move-deeper
	  (function
	   (lambda (elt)
	     (cond ((and (string-match regexp (car elt))
			 (or (eq ord 1) (match-end 2)))
		    (setq head (substring (car elt) 0 (match-end 1))
			  tail (if (match-end 2) (substring (car elt)
							    (match-end 2)))
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
		    (setq root-packages (cons elt root-packages))))))))
    (setcdr to l1)			; Init to dynamic space
    (setq writeto to)
    (setq ord 1)
    (mapcar move-deeper packages)
    (setq ord 2)
    (mapcar move-deeper methods)
    (if recurse
	(mapcar (function (lambda (elt)
			  (cperl-tags-treeify elt (1+ level))))
		(cdr to)))
    ;;Now clean up leaders with one child only
    (mapcar (function (lambda (elt)
			(if (not (and (listp (cdr elt))
				      (eq (length elt) 2))) nil
			    (setcar elt (car (nth 1 elt)))
			    (setcdr elt (cdr (nth 1 elt))))))
	    (cdr to))
    ;; Sort the roots of subtrees
    (if (default-value 'imenu-sort-function)
	(setcdr to
		(sort (cdr to) (default-value 'imenu-sort-function))))
    ;; Now add back functions removed from display
    (mapcar (function (lambda (elt)
			(setcdr to (cons elt (cdr to)))))
	    (if (default-value 'imenu-sort-function)
		(nreverse
		 (sort root-functions (default-value 'imenu-sort-function)))
	      root-functions))
    ;; Now add back packages removed from display
    (mapcar (function (lambda (elt)
			(setcdr to (cons (cons (concat "package " (car elt))
					       (cdr elt))
					 (cdr to)))))
	    (if (default-value 'imenu-sort-function)
		(nreverse
		 (sort root-packages (default-value 'imenu-sort-function)))
	      root-packages))
    ))

;;;(x-popup-menu t
;;;   '(keymap "Name1"
;;;	    ("Ret1" "aa")
;;;	    ("Head1" "ab"
;;;	     keymap "Name2"
;;;	     ("Tail1" "x") ("Tail2" "y"))))

(defun cperl-list-fold (list name limit)
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

(defun cperl-menu-to-keymap (menu &optional name)
  (let (list)
    (cons 'keymap
	  (mapcar
	   (function
	    (lambda (elt)
	      (cond ((listp (cdr elt))
		     (setq list (cperl-list-fold
				 (cdr elt) (car elt) imenu-max-items))
		     (cons nil
			   (cons (car elt)
				 (cperl-menu-to-keymap list))))
		    (t
		     (list (cdr elt) (car elt) t))))) ; t is needed in 19.34
	   (cperl-list-fold menu "Root" imenu-max-items)))))


(defvar cperl-bad-style-regexp
  (mapconcat 'identity
   '("[^-\n\t <>=+!.&|(*/'`\"#^][-=+<>!|&^]" ; char sign
     "[-<>=+^&|]+[^- \t\n=+<>~]"	; sign+ char
     )
   "\\|")
  "Finds places such that insertion of a whitespace may help a lot.")

(defvar cperl-not-bad-style-regexp
  (mapconcat 'identity
   '("[^-\t <>=+]\\(--\\|\\+\\+\\)"	; var-- var++
     "[a-zA-Z0-9_][|&][a-zA-Z0-9_$]"	; abc|def abc&def are often used.
     "&[(a-zA-Z0-9_$]"			; &subroutine &(var->field)
     "<\\$?\\sw+\\(\\.\\sw+\\)?>"	; <IN> <stdin.h>
     "-[a-zA-Z][ \t]+[_$\"'`a-zA-Z]"	; -f file, -t STDIN
     "-[0-9]"				; -5
     "\\+\\+"				; ++var
     "--"				; --var
     ".->"				; a->b
     "->"				; a SPACE ->b
     "\\[-"				; a[-1]
     "\\\\[&$@*\\\\]"			; \&func
     "^="				; =head
     "\\$."				; $|
     "<<[a-zA-Z_'\"`]"			; <<FOO, <<'FOO'
     "||"
     "&&"
     "[CBIXSLFZ]<\\(\\sw\\|\\s \\|\\s_\\|[\n]\\)*>" ; C<code like text>
     "-[a-zA-Z_0-9]+[ \t]*=>"			; -option => value
     ;; Unaddressed trouble spots: = -abc, f(56, -abc) --- specialcased below
     ;;"[*/+-|&<.]+="
     )
   "\\|")
  "If matches at the start of match found by `my-bad-c-style-regexp',
insertion of a whitespace will not help.")

(defvar found-bad)

(defun cperl-find-bad-style ()
  "Find places in the buffer where insertion of a whitespace may help.
Prompts user for insertion of spaces.
Currently it is tuned to C and Perl syntax."
  (interactive)
  (let (found-bad (p (point)))
    (setq last-nonmenu-event 13)	; To disable popup
    (beginning-of-buffer)
    (map-y-or-n-p "Insert space here? "
		  (function (lambda (arg) (insert " ")))
		  'cperl-next-bad-style
		  '("location" "locations" "insert a space into")
		  '((?\C-r (lambda (arg)
			     (let ((buffer-quit-function
				    'exit-recursive-edit))
			       (message "Exit with Esc Esc")
			       (recursive-edit)
			       t))	; Consider acted upon
			   "edit, exit with Esc Esc")
		    (?e (lambda (arg)
			  (let ((buffer-quit-function
				 'exit-recursive-edit))
			    (message "Exit with Esc Esc")
			    (recursive-edit)
			    t))		; Consider acted upon
			"edit, exit with Esc Esc"))
		  t)
    (if found-bad (goto-char found-bad)
      (goto-char p)
      (message "No appropriate place found"))))

(defun cperl-next-bad-style ()
  (let (p (not-found t) (point (point)) found)
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
		  (memq (preceding-char) '(?\= ?\> ?\< ?\, ?\(, ?\[, ?\{))))
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
   'identity
   '("[$@%*&][0-9a-zA-Z_:]+\\([ \t]*[[{]\\)?"		; Usual variable
     "[$@]\\^[a-zA-Z]"			; Special variable
     "[$@][^ \n\t]"			; Special variable
     "-[a-zA-Z]"			; File test
     "\\\\[a-zA-Z0]"			; Special chars
     "^=[a-z][a-zA-Z0-9_]*"		; Pod sections
     "[-!&*+,-./<=>?\\\\^|~]+"		; Operator
     "[a-zA-Z_0-9:]+"			; symbol or number
     "x="
     "#!"
     )
   ;;"\\)\\|\\("
   "\\|"
   )
	  ;;"\\)"
	  ;;)
  "Matches places in the buffer we can find help for.")

(defvar cperl-message-on-help-error t)
(defvar cperl-help-from-timer nil)

(defun cperl-word-at-point-hard ()
  ;; Does not save-excursion
  ;; Get to the something meaningful
  (or (eobp) (eolp) (forward-char 1))
  (re-search-backward "[-a-zA-Z0-9_:!&*+,-./<=>?\\\\^|~$%@]"
		      (save-excursion (beginning-of-line) (point))
		      'to-beg)
  ;;  (cond
  ;;   ((or (eobp) (looking-at "[][ \t\n{}();,]")) ; Not at a symbol
  ;;    (skip-chars-backward " \n\t\r({[]});,")
  ;;    (or (bobp) (backward-char 1))))
  ;; Try to backtrace
  (cond
   ((looking-at "[a-zA-Z0-9_:]")	; symbol
    (skip-chars-backward "a-zA-Z0-9_:")
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
	     (looking-at "\\$?[a-zA-Z0-9_:]+>")) ; <FH>
	(forward-char -1)))
   ((and (looking-at "=") (eq (preceding-char) ?x)) ; x=
    (forward-char -1))
   ((and (looking-at "\\^") (eq (preceding-char) ?\$)) ; $^I
    (forward-char -1))
   ((looking-at "[-!&*+,-./<=>?\\\\^|~]")
    (skip-chars-backward "-!&*+,-./<=>?\\\\^|~")
    (cond
     ((and (eq (preceding-char) ?\$)
	   (not (eq (char-after (- (point) 2)) ?\$))) ; $-
      (forward-char -1))
     ((and (eq (following-char) ?\>)
	   (string-match "[a-zA-Z0-9_]" (char-to-string (preceding-char)))
	   (save-excursion
	     (forward-sexp -1)
	     (and (eq (preceding-char) ?\<)
		  (looking-at "\\$?[a-zA-Z0-9_:]+>")))) ; <FH>
      (search-backward "<"))))
   ((and (eq (following-char) ?\$)
	 (eq (preceding-char) ?\<)
	 (looking-at "\\$?[a-zA-Z0-9_:]+>")) ; <$fh>
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
  "Display the documentation of symbol at point, a Perl operator."
  (let ((enable-recursive-minibuffers t)
	args-file regexp)
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

(defvar cperl-short-docs "Ignore my value"
  ;; Perl4 version was written by Johan Vromans (jvromans@squirrel.nl)
  "# based on '@(#)@ perl-descr.el 1.9 - describe-perl-symbol' [Perl 5]
! ...	Logical negation.
... != ...	Numeric inequality.
... !~ ...	Search pattern, substitution, or translation (negated).
$!	In numeric context: errno.  In a string context: error string.
$\"	The separator which joins elements of arrays interpolated in strings.
$#	The output format for printed numbers.  Initial value is %.15g or close.
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
$'	The string after what was matched by the last match (auto-local).
$`	The string before what was matched by the last match (auto-local).

$(	The real gid of this process.
$)	The effective gid of this process.
$*	Deprecated: Set to 1 to do multiline matching within a string.
$+	The last bracket matched by the last search pattern.
$,	The output field separator for the print operator.
$-	The number of lines left on the page.
$.	The current input line number of the last filehandle that was read.
$/	The input record separator, newline by default.
$0	Name of the file containing the perl script being executed.  May be set.
$:     String may be broken after these characters to fill ^-lines in a format.
$;	Subscript separator for multi-dim array emulation.  Default \"\\034\".
$<	The real uid of this process.
$=	The page length of the current output channel.  Default is 60 lines.
$>	The effective uid of this process.
$?	The status returned by the last ``, pipe close or `system'.
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
$^L     What formats output to perform a formfeed.  Default is \f.
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
*NAME	Glob: all objects refered by NAME.  *NAM1 = *NAM2 aliases NAM1 to NAM2.
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
..	Alternation, also range operator.
.=	Concatenate assignment strings
... / ...	Division.	/PATTERN/ioxsmg	Pattern match
... /= ...	Division assignment.
/PATTERN/ioxsmg	Pattern match.
... < ...	Numeric less than.	<pattern>	Glob.	See <NAME>, <> as well.
<NAME>	Reads line from filehandle NAME (a bareword or dollar-bareword).
<pattern>	Glob (Unless pattern is bareword/dollar-bareword - see <NAME>).
<>	Reads line from union of files in @ARGV (= command line) and STDIN.
... << ...	Bitwise shift left.	<<	start of HERE-DOCUMENT.
... <= ...	Numeric less than or equal to.
... <=> ...	Numeric compare.
... = ...	Assignment.
... == ...	Numeric equality.
... =~ ...	Search pattern, substitution, or translation
... > ...	Numeric greater than.
... >= ...	Numeric greater than or equal to.
... >> ...	Bitwise shift right.
... >>= ...	Bitwise shift right assignment.
... ? ... : ...	Condition=if-then-else operator.   ?PAT? One-time pattern match.
?PATTERN?	One-time pattern match.
@ARGV	Command line arguments (not including the command name - see $0).
@INC	List of places to look for perl scripts during do/include/use.
@_	Parameter array for subroutines.  Also used by split unless in array context.
\\  Creates reference to what follows, like \$var, or quotes non-\w in strings.
\\0	Octal char, e.g. \\033.
\\E	Case modification terminator.  See \\Q, \\L, and \\U.
\\L	Lowercase until \\E .  See also \l, lc.
\\U	Upcase until \\E .  See also \u, uc.
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
__END__	Ends program source.
__DATA__	Ends program source.
__FILE__	Current (source) filename.
__LINE__	Current line in current source.
__PACKAGE__	Current package.
ARGV	Default multi-file input filehandle.  <ARGV> is a synonym for <>.
ARGVOUT	Output filehandle with -i flag.
BEGIN { ... }	Immediately executed (during compilation) piece of code.
END { ... }	Pseudo-subroutine executed after the script finishes.
CHECK { ... }	Pseudo-subroutine executed after the script is compiled.
INIT { ... }	Pseudo-subroutine executed before the script starts running.
DATA	Input filehandle for what follows after __END__	or __DATA__.
accept(NEWSOCKET,GENERICSOCKET)
alarm(SECONDS)
atan2(X,Y)
bind(SOCKET,NAME)
binmode(FILEHANDLE)
caller[(LEVEL)]
chdir(EXPR)
chmod(LIST)
chop[(LIST|VAR)]
chown(LIST)
chroot(FILENAME)
close(FILEHANDLE)
closedir(DIRHANDLE)
... cmp ...	String compare.
connect(SOCKET,NAME)
continue of { block } continue { block }.  Is executed after `next' or at end.
cos(EXPR)
crypt(PLAINTEXT,SALT)
dbmclose(%HASH)
dbmopen(%HASH,DBNAME,MODE)
defined(EXPR)
delete($HASH{KEY})
die(LIST)
do { ... }|SUBR while|until EXPR	executes at least once
do(EXPR|SUBR([LIST]))	(with while|until executes at least once)
dump LABEL
each(%HASH)
endgrent
endhostent
endnetent
endprotoent
endpwent
endservent
eof[([FILEHANDLE])]
... eq ...	String equality.
eval(EXPR) or eval { BLOCK }
exec(LIST)
exit(EXPR)
exp(EXPR)
fcntl(FILEHANDLE,FUNCTION,SCALAR)
fileno(FILEHANDLE)
flock(FILEHANDLE,OPERATION)
for (EXPR;EXPR;EXPR) { ... }
foreach [VAR] (@ARRAY) { ... }
fork
... ge ...	String greater than or equal.
getc[(FILEHANDLE)]
getgrent
getgrgid(GID)
getgrnam(NAME)
gethostbyaddr(ADDR,ADDRTYPE)
gethostbyname(NAME)
gethostent
getlogin
getnetbyaddr(ADDR,ADDRTYPE)
getnetbyname(NAME)
getnetent
getpeername(SOCKET)
getpgrp(PID)
getppid
getpriority(WHICH,WHO)
getprotobyname(NAME)
getprotobynumber(NUMBER)
getprotoent
getpwent
getpwnam(NAME)
getpwuid(UID)
getservbyname(NAME,PROTO)
getservbyport(PORT,PROTO)
getservent
getsockname(SOCKET)
getsockopt(SOCKET,LEVEL,OPTNAME)
gmtime(EXPR)
goto LABEL
... gt ...	String greater than.
hex(EXPR)
if (EXPR) { ... } [ elsif (EXPR) { ... } ... ] [ else { ... } ] or EXPR if EXPR
index(STR,SUBSTR[,OFFSET])
int(EXPR)
ioctl(FILEHANDLE,FUNCTION,SCALAR)
join(EXPR,LIST)
keys(%HASH)
kill(LIST)
last [LABEL]
... le ...	String less than or equal.
length(EXPR)
link(OLDFILE,NEWFILE)
listen(SOCKET,QUEUESIZE)
local(LIST)
localtime(EXPR)
log(EXPR)
lstat(EXPR|FILEHANDLE|VAR)
... lt ...	String less than.
m/PATTERN/iogsmx
mkdir(FILENAME,MODE)
msgctl(ID,CMD,ARG)
msgget(KEY,FLAGS)
msgrcv(ID,VAR,SIZE,TYPE.FLAGS)
msgsnd(ID,MSG,FLAGS)
my VAR or my (VAR1,...)	Introduces a lexical variable ($VAR, @ARR, or %HASH).
our VAR or our (VAR1,...) Lexically enable a global variable ($V, @A, or %H).
... ne ...	String inequality.
next [LABEL]
oct(EXPR)
open(FILEHANDLE[,EXPR])
opendir(DIRHANDLE,EXPR)
ord(EXPR)	ASCII value of the first char of the string.
pack(TEMPLATE,LIST)
package NAME	Introduces package context.
pipe(READHANDLE,WRITEHANDLE)	Create a pair of filehandles on ends of a pipe.
pop(ARRAY)
print [FILEHANDLE] [(LIST)]
printf [FILEHANDLE] (FORMAT,LIST)
push(ARRAY,LIST)
q/STRING/	Synonym for 'STRING'
qq/STRING/	Synonym for \"STRING\"
qx/STRING/	Synonym for `STRING`
rand[(EXPR)]
read(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
readdir(DIRHANDLE)
readlink(EXPR)
recv(SOCKET,SCALAR,LEN,FLAGS)
redo [LABEL]
rename(OLDNAME,NEWNAME)
require [FILENAME | PERL_VERSION]
reset[(EXPR)]
return(LIST)
reverse(LIST)
rewinddir(DIRHANDLE)
rindex(STR,SUBSTR[,OFFSET])
rmdir(FILENAME)
s/PATTERN/REPLACEMENT/gieoxsm
scalar(EXPR)
seek(FILEHANDLE,POSITION,WHENCE)
seekdir(DIRHANDLE,POS)
select(FILEHANDLE | RBITS,WBITS,EBITS,TIMEOUT)
semctl(ID,SEMNUM,CMD,ARG)
semget(KEY,NSEMS,SIZE,FLAGS)
semop(KEY,...)
send(SOCKET,MSG,FLAGS[,TO])
setgrent
sethostent(STAYOPEN)
setnetent(STAYOPEN)
setpgrp(PID,PGRP)
setpriority(WHICH,WHO,PRIORITY)
setprotoent(STAYOPEN)
setpwent
setservent(STAYOPEN)
setsockopt(SOCKET,LEVEL,OPTNAME,OPTVAL)
shift[(ARRAY)]
shmctl(ID,CMD,ARG)
shmget(KEY,SIZE,FLAGS)
shmread(ID,VAR,POS,SIZE)
shmwrite(ID,STRING,POS,SIZE)
shutdown(SOCKET,HOW)
sin(EXPR)
sleep[(EXPR)]
socket(SOCKET,DOMAIN,TYPE,PROTOCOL)
socketpair(SOCKET1,SOCKET2,DOMAIN,TYPE,PROTOCOL)
sort [SUBROUTINE] (LIST)
splice(ARRAY,OFFSET[,LENGTH[,LIST]])
split[(/PATTERN/[,EXPR[,LIMIT]])]
sprintf(FORMAT,LIST)
sqrt(EXPR)
srand(EXPR)
stat(EXPR|FILEHANDLE|VAR)
study[(SCALAR)]
sub [NAME [(format)]] { BODY }	sub NAME [(format)];	sub [(format)] {...}
substr(EXPR,OFFSET[,LEN])
symlink(OLDFILE,NEWFILE)
syscall(LIST)
sysread(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
system(LIST)
syswrite(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
tell[(FILEHANDLE)]
telldir(DIRHANDLE)
time
times
tr/SEARCHLIST/REPLACEMENTLIST/cds
truncate(FILE|EXPR,LENGTH)
umask[(EXPR)]
undef[(EXPR)]
unless (EXPR) { ... } [ else { ... } ] or EXPR unless EXPR
unlink(LIST)
unpack(TEMPLATE,EXPR)
unshift(ARRAY,LIST)
until (EXPR) { ... }					EXPR until EXPR
utime(LIST)
values(%HASH)
vec(EXPR,OFFSET,BITS)
wait
waitpid(PID,FLAGS)
wantarray	Returns true if the sub/eval is called in list context.
warn(LIST)
while  (EXPR) { ... }					EXPR while EXPR
write[(EXPR|FILEHANDLE)]
... x ...	Repeat string or array.
x= ...	Repetition assignment.
y/SEARCHLIST/REPLACEMENTLIST/
... | ...	Bitwise or.
... || ...	Logical or.
~ ...		Unary bitwise complement.
#!	OS interpreter indicator.  If contains `perl', used for options, and -x.
AUTOLOAD {...}	Shorthand for `sub AUTOLOAD {...}'.
CORE::		Prefix to access builtin function if imported sub obscures it.
SUPER::		Prefix to lookup for a method in @ISA classes.
DESTROY		Shorthand for `sub DESTROY {...}'.
... EQ ...	Obsolete synonym of `eq'.
... GE ...	Obsolete synonym of `ge'.
... GT ...	Obsolete synonym of `gt'.
... LE ...	Obsolete synonym of `le'.
... LT ...	Obsolete synonym of `lt'.
... NE ...	Obsolete synonym of `ne'.
abs [ EXPR ]	absolute value
... and ...		Low-precedence synonym for &&.
bless REFERENCE [, PACKAGE]	Makes reference into an object of a package.
chomp [LIST]	Strips $/ off LIST/$_.  Returns count.  Special if $/ eq ''!
chr		Converts a number to char with the same ordinal.
else		Part of if/unless {BLOCK} elsif {BLOCK} else {BLOCK}.
elsif		Part of if/unless {BLOCK} elsif {BLOCK} else {BLOCK}.
exists	$HASH{KEY}	True if the key exists.
format [NAME] =	 Start of output format.  Ended by a single dot (.) on a line.
formline PICTURE, LIST	Backdoor into \"format\" processing.
glob EXPR	Synonym of <EXPR>.
lc [ EXPR ]	Returns lowercased EXPR.
lcfirst [ EXPR ]	Returns EXPR with lower-cased first letter.
grep EXPR,LIST  or grep {BLOCK} LIST	Filters LIST via EXPR/BLOCK.
map EXPR, LIST	or map {BLOCK} LIST	Applies EXPR/BLOCK to elts of LIST.
no PACKAGE [SYMBOL1, ...]  Partial reverse for `use'.  Runs `unimport' method.
not ...		Low-precedence synonym for ! - negation.
... or ...		Low-precedence synonym for ||.
pos STRING    Set/Get end-position of the last match over this string, see \\G.
quotemeta [ EXPR ]	Quote regexp metacharacters.
qw/WORD1 .../		Synonym of split('', 'WORD1 ...')
readline FH	Synonym of <FH>.
readpipe CMD	Synonym of `CMD`.
ref [ EXPR ]	Type of EXPR when dereferenced.
sysopen FH, FILENAME, MODE [, PERM]	(MODE is numeric, see Fcntl.)
tie VAR, PACKAGE, LIST	Hide an object behind a simple Perl variable.
tied		Returns internal object for a tied data.
uc [ EXPR ]	Returns upcased EXPR.
ucfirst [ EXPR ]	Returns EXPR with upcased first letter.
untie VAR	Unlink an object from a simple Perl variable.
use PACKAGE [SYMBOL1, ...]  Compile-time `require' with consequent `import'.
... xor ...		Low-precedence synonym for exclusive or.
prototype \&SUB	Returns prototype of the function given a reference.
=head1		Top-level heading.
=head2		Second-level heading.
=head3		Third-level heading (is there such?).
=over [ NUMBER ]	Start list.
=item [ TITLE ]		Start new item in the list.
=back		End list.
=cut		Switch from POD to Perl.
=pod		Switch from Perl to POD.
")

(defun cperl-switch-to-doc-buffer ()
  "Go to the perl documentation buffer and insert the documentation."
  (interactive)
  (let ((buf (get-buffer-create cperl-doc-buffer)))
    (if (interactive-p)
	(switch-to-buffer-other-window buf)
      (set-buffer buf))
    (if (= (buffer-size) 0)
	(progn
	  (insert (documentation-property 'cperl-short-docs
					  'variable-documentation))
	  (setq buffer-read-only t)))))

(defun cperl-beautify-regexp-piece (b e embed level)
  ;; b is before the starting delimiter, e before the ending
  ;; e should be a marker, may be changed, but remains "correct".
  ;; EMBED is nil iff we process the whole REx.
  ;; The REx is guarantied to have //x
  ;; LEVEL shows how many levels deep to go
  ;; position at enter and at leave is not defined
  (let (s c tmp (m (make-marker)) (m1 (make-marker)) c1 spaces inline code pos)
    (if (not embed)
	(goto-char (1+ b))
      (goto-char b)
      (cond ((looking-at "(\\?\\\\#")	;  (?#) wrongly commented when //x-ing
	     (forward-char 2)
	     (delete-char 1)
	     (forward-char 1))
	    ((looking-at "(\\?[^a-zA-Z]")
	     (forward-char 3))
	    ((looking-at "(\\?")	; (?i)
	     (forward-char 2))
	    (t
	     (forward-char 1))))
    (setq c (if embed (current-indentation) (1- (current-column)))
	  c1 (+ c (or cperl-regexp-indent-step cperl-indent-level)))
    (or (looking-at "[ \t]*[\n#]")
	(progn
	  (insert "\n")))
    (goto-char e)
    (beginning-of-line)
    (if (re-search-forward "[^ \t]" e t)
	(progn				; Something before the ending delimiter
	  (goto-char e)
	  (delete-horizontal-space)
	  (insert "\n")
	  (indent-to-column c)
	  (set-marker e (point))))
    (goto-char b)
    (end-of-line 2)
    (while (< (point) (marker-position e))
      (beginning-of-line)
      (setq s (point)
	    inline t)
      (skip-chars-forward " \t")
      (delete-region s (point))
      (indent-to-column c1)
      (while (and
	      inline
	      (looking-at
	       (concat "\\([a-zA-Z0-9]+[^*+{?]\\)" ; 1 word
		       "\\|"		; Embedded variable
		       "\\$\\([a-zA-Z0-9_]+\\([[{]\\)?\\|[^\n \t)|]\\)" ; 2 3
		       "\\|"		; $ ^
		       "[$^]"
		       "\\|"		; simple-code simple-code*?
		       "\\(\\\\.\\|[^][()#|*+?\n]\\)\\([*+{?]\\??\\)?" ; 4 5
		       "\\|"		; Class
		       "\\(\\[\\)"	; 6
		       "\\|"		; Grouping
		       "\\((\\(\\?\\)?\\)" ; 7 8
		       "\\|"		; |
		       "\\(|\\)"	; 9
		       )))
	(goto-char (match-end 0))
	(setq spaces t)
	(cond ((match-beginning 1)	; Alphanum word + junk
	       (forward-char -1))
	      ((or (match-beginning 3)	; $ab[12]
		   (and (match-beginning 5) ; X* X+ X{2,3}
			(eq (preceding-char) ?\{)))
	       (forward-char -1)
	       (forward-sexp 1))
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
	       (if (eq (following-char) ?\{)
		   (progn
		     (forward-sexp 1)
		     (and (eq (following-char) ??)
			  (forward-char 1)))
		 (re-search-forward "\\=\\([*+?]\\??\\)" e t)))
	      ((match-beginning 7)	; ()
	       (goto-char (match-beginning 0))
	       (setq pos (current-column))
	       (or (eq pos c1)
		   (progn
		     (delete-horizontal-space)
		     (insert "\n")
		     (indent-to-column c1)))
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
	       (indent-to-column c)
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
	    (error "unknown code \"%s\" in a regexp"
		   (buffer-substring (point) (1+ (point)))))
	(and inline (end-of-line 2)))
    ;; Special-case the last line of group
    (if (and (>= (point) (marker-position e))
	     (/= (current-indentation) c))
	(progn
	 (beginning-of-line)
	 (setq s (point))
	 (skip-chars-forward " \t")
	 (delete-region s (point))
	 (indent-to-column c)))
  ))

(defun cperl-make-regexp-x ()
  ;; Returns position of the start
  ;; XXX this is called too often!  Need to cache the result!
  (save-excursion
    (or cperl-use-syntax-table-text-property
	(error "I need to have a regexp marked!"))
    ;; Find the start
    (if (looking-at "\\s|")
	nil				; good already
      (if (looking-at "\\([smy]\\|qr\\)\\s|")
	  (forward-char 1)
	(re-search-backward "\\s|")))		; Assume it is scanned already.
    ;;(forward-char 1)
    (let ((b (point)) (e (make-marker)) have-x delim (c (current-column))
	  (sub-p (eq (preceding-char) ?s)) s)
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
	(while (re-search-forward "\\(\\=\\|[^\\\\]\\)\\(\\\\\\\\\\)*[ \t\n#]" e t) ; Need to include (?#) too?
	  (forward-char -1)
	  (insert "\\")
	  (forward-char 1)))
      b)))

(defun cperl-beautify-regexp (&optional deep)
  "do it.  (Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive "P")
  (if deep
      (prefix-numeric-value deep)
    (setq deep -1))
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
	      (skip-chars-backward "\\")
	      (looking-at "\\(\\\\\\\\\\)*(")))
      (or done (forward-char -1)))))

(defun cperl-contract-level ()
  "Find an enclosing group in regexp and contract it.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive)
  ;; (save-excursion		; Can't, breaks `cperl-contract-levels'
    (cperl-regext-to-level-start)
    (let ((b (point)) (e (make-marker)) s c)
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (goto-char b)
      (while (re-search-forward "\\(#\\)\\|\n" e 'to-end)
	(cond 
	 ((match-beginning 1)		; #-comment
	  (or c (setq c (current-indentation)))
	  (beginning-of-line 2)		; Skip
	  (setq s (point))
	  (skip-chars-forward " \t")
	  (delete-region s (point))
	  (indent-to-column c))
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
    (let ((b (point)) (e (make-marker)) s c)
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
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive "P")
  (if deep
      (prefix-numeric-value deep)
    (setq deep -1))
  (save-excursion
    (cperl-regext-to-level-start)
    (let ((b (point)) (e (make-marker)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (cperl-beautify-regexp-piece b e nil deep))))

(defun cperl-invert-if-unless ()
  "Change `if (A) {B}' into `B if A;' etc if possible."
  (interactive)
  (or (looking-at "\\<")
	(forward-sexp -1))
  (if (looking-at "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>")
      (let ((pos1 (point))
	    pos2 pos3 pos4 pos5 s1 s2 state p pos45
	    (s0 (buffer-substring (match-beginning 0) (match-end 0))))
	(forward-sexp 2)
	(setq pos3 (point))
	(forward-sexp -1)
	(setq pos2 (point))
	(if (eq (following-char) ?\( )
	    (progn
	      (goto-char pos3)
	      (forward-sexp 1)
	      (setq pos5 (point))
	      (forward-sexp -1)
	      (setq pos4 (point))
	      ;; XXXX In fact may be `A if (B); {C}' ...
	      (if (and (eq (following-char) ?\{ )
		       (progn
			 (cperl-backward-to-noncomment pos3)
			 (eq (preceding-char) ?\) )))
		  (if (condition-case nil
			  (progn
			    (goto-char pos5)
			    (forward-sexp 1)
			    (forward-sexp -1)
			    (looking-at "\\<els\\(e\\|if\\)\\>"))
			(error nil))
		      (error
		       "`%s' (EXPR) {BLOCK} with `else'/`elsif'" s0)
		    (goto-char (1- pos5))
		    (cperl-backward-to-noncomment pos4)
		    (if (eq (preceding-char) ?\;)
			(forward-char -1))
		    (setq pos45 (point))
		    (goto-char pos4)
		    (while (re-search-forward "\\<\\(for\\|foreach\\|if\\|unless\\|while\\|until\\)\\>\\|;" pos45 t)
		      (setq p (match-beginning 0)
			    s1 (buffer-substring p (match-end 0))
			    state (parse-partial-sexp pos4 p))
		      (or (nth 3 state)
			  (nth 4 state)
			  (nth 5 state)
			  (error "`%s' inside `%s' BLOCK" s1 s0))
		      (goto-char (match-end 0)))
		    ;; Finally got it
		    (goto-char (1+ pos4))
		    (skip-chars-forward " \t\n")
		    (setq s2 (buffer-substring (point) pos45))
		    (goto-char pos45)
		    (or (looking-at ";?[ \t\n]*}")
			(progn
			  (skip-chars-forward "; \t\n")
			  (setq s2 (concat s2 "\n" (buffer-substring (point) (1- pos5))))))
		    (and (equal s2 "")
			 (setq s2 "1"))
		    (goto-char (1- pos3))
		    (cperl-backward-to-noncomment pos2)
		    (or (looking-at "[ \t\n]*)")
			(goto-char (1- pos3)))
		    (setq p (point))
		    (goto-char (1+ pos2))
		    (skip-chars-forward " \t\n")
		    (setq s1 (buffer-substring (point) p))
		    (delete-region pos4 pos5)
		    (delete-region pos2 pos3)
		    (goto-char pos1)
		    (insert s2 " ")
		    (just-one-space)
		    (forward-word 1)
		    (setq pos1 (point))
		    (insert " " s1 ";")
		    (delete-horizontal-space)
		    (forward-char -1)
		    (delete-horizontal-space)
		    (goto-char pos1)
		    (just-one-space)
		    (cperl-indent-line))
		(error "`%s' (EXPR) not with an {BLOCK}" s0)))
	  (error "`%s' not with an (EXPR)" s0)))
    (error "Not at `if', `unless', `while', `unless', `for' or `foreach'")))

;;; By Anthony Foiani <afoiani@uswest.com>
;;; Getting help on modules in C-h f ?
;;; This is a modified version of `man'.
;;; Need to teach it how to lookup functions
(defun cperl-perldoc (word)
  "Run `perldoc' on WORD."
  (interactive
   (list (let* ((default-entry (cperl-word-at-point))
                (input (read-string
                        (format "perldoc entry%s: "
                                (if (string= default-entry "")
                                    ""
                                  (format " (default %s)" default-entry))))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No perldoc args given")
                 default-entry)
             input))))
  (require 'man)
  (let* ((is-func (and
		   (string-match "^[a-z]+$" word)
		   (string-match (concat "^" word "\\>")
				 (documentation-property
				  'cperl-short-docs
				  'variable-documentation))))
	 (manual-program (if is-func "perldoc -f" "perldoc")))
    (Man-getpage-in-background word)))

(defun cperl-perldoc-at-point ()
  "Run a `perldoc' on the word around point."
  (interactive)
  (cperl-perldoc (cperl-word-at-point)))

(defcustom pod2man-program "pod2man"
  "*File name for `pod2man'."
  :type 'file
  :group 'cperl)

;;; By Nick Roberts <Nick.Roberts@src.bae.co.uk> (with changes)
(defun cperl-pod-to-manpage ()
  "Create a virtual manpage in Emacs from the Perl Online Documentation."
  (interactive)
  (require 'man)
  (let* ((pod2man-args (concat buffer-file-name " | nroff -man "))
	 (bufname (concat "Man " buffer-file-name))
	 (buffer (generate-new-buffer bufname)))
    (save-excursion
      (set-buffer buffer)
      (let ((process-environment (copy-sequence process-environment)))
        ;; Prevent any attempt to use display terminal fanciness.
        (setenv "TERM" "dumb")
        (set-process-sentinel
         (start-process pod2man-program buffer "sh" "-c"
                        (format (cperl-pod2man-build-command) pod2man-args))
         'Man-bgproc-sentinel)))))

(defun cperl-pod2man-build-command ()
  "Builds the entire background manpage and cleaning command."
  (let ((command (concat pod2man-program " %s 2>/dev/null"))
        (flist Man-filter-list))
    (while (and flist (car flist))
      (let ((pcom (car (car flist)))
            (pargs (cdr (car flist))))
        (setq command
              (concat command " | " pcom " "
                      (mapconcat '(lambda (phrase)
                                    (if (not (stringp phrase))
                                        (error "Malformed Man-filter-list"))
                                    phrase)
                                 pargs " ")))
        (setq flist (cdr flist))))
    command))

(defun cperl-lazy-install ())		; Avoid a warning

(if (fboundp 'run-with-idle-timer)
    (progn
      (defvar cperl-help-shown nil
	"Non-nil means that the help was already shown now.")

      (defvar cperl-lazy-installed nil
	"Non-nil means that the lazy-help handlers are installed now.")

      (defun cperl-lazy-install ()
	(interactive)
	(make-variable-buffer-local 'cperl-help-shown)
	(if (and (cperl-val 'cperl-lazy-help-time)
		 (not cperl-lazy-installed))
	    (progn
	      (add-hook 'post-command-hook 'cperl-lazy-hook)
	      (run-with-idle-timer
	       (cperl-val 'cperl-lazy-help-time 1000000 5)
	       t
	       'cperl-get-help-defer)
	      (setq cperl-lazy-installed t))))

      (defun cperl-lazy-unstall ()
	(interactive)
	(remove-hook 'post-command-hook 'cperl-lazy-hook)
	(cancel-function-timers 'cperl-get-help-defer)
	(setq cperl-lazy-installed nil))

      (defun cperl-lazy-hook ()
	(setq cperl-help-shown nil))

      (defun cperl-get-help-defer ()
	(when (memq major-mode '(perl-mode cperl-mode))
	  (let ((cperl-message-on-help-error nil) (cperl-help-from-timer t))
	    (cperl-get-help)
	    (setq cperl-help-shown t))))
      (cperl-lazy-install)))


;;; Plug for wrong font-lock:

(defun cperl-font-lock-unfontify-region-function (beg end)
  (let* ((modified (buffer-modified-p)) (buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 before-change-functions after-change-functions
	 deactivate-mark buffer-file-name buffer-file-truename)
    (remove-text-properties beg end '(face nil))
    (when (and (not modified) (buffer-modified-p))
      (set-buffer-modified-p nil))))

(defvar cperl-d-l nil)
(defun cperl-fontify-syntaxically (end)
  ;; Some vars for debugging only
  ;; (message "Syntaxifying...")
  (let (start (dbg (point)) (iend end) 
	(istate (car cperl-syntax-state)))
    (and cperl-syntaxify-unwind
	 (setq end (cperl-unwind-to-safe t end)))
    (setq start (point))
    (or cperl-syntax-done-to
	(setq cperl-syntax-done-to (point-min)))
    (if (or (not (boundp 'font-lock-hot-pass))
	    (eval 'font-lock-hot-pass)
	    t)				; Not debugged otherwise
	;; Need to forget what is after `start'
	(setq start (min cperl-syntax-done-to start))
      ;; Fontification without a change
      (setq start (max cperl-syntax-done-to start)))
    (and (> end start)
	 (setq cperl-syntax-done-to start) ; In case what follows fails
	 (cperl-find-pods-heres start end t nil t))
    (if (eq cperl-syntaxify-by-font-lock 'message)
	(message "Syntaxified %s..%s from %s to %s(%s), state %s-->%s"
		 dbg iend
		 start end cperl-syntax-done-to
		 istate (car cperl-syntax-state))) ; For debugging
    nil))					; Do not iterate

(defun cperl-fontify-update (end)
  (let ((pos (point)) prop posend)
    (while (< pos end)
      (setq prop (get-text-property pos 'cperl-postpone))
      (setq posend (next-single-property-change pos 'cperl-postpone nil end))
      (and prop (put-text-property pos posend (car prop) (cdr prop)))
      (setq pos posend)))
  nil)				; Do not iterate

(defun cperl-update-syntaxification (from to)
  (if (and cperl-use-syntax-table-text-property
	   cperl-syntaxify-by-font-lock
	   (or (null cperl-syntax-done-to)
	       (< cperl-syntax-done-to to)))
      (progn
	(save-excursion
	  (goto-char from)
	  (cperl-fontify-syntaxically to)))))

(defvar cperl-version
  (let ((v  "Revision: 4.32"))
    (string-match ":\\s *\\([0-9.]+\\)" v)
    (substring v (match-beginning 1) (match-end 1)))
  "Version of IZ-supported CPerl package this file is based on.")

(provide 'cperl-mode)

;;; cperl-mode.el ends here
