;;; antlr-mode.el --- major mode for ANTLR grammar files

;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
;;
;; Author: Christoph.Wedler@sap.com
;; Keywords: languages
;; Version: 1.4
;; X-URL: http://www.fmi.uni-passau.de/~wedler/antlr-mode/

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

;; Major mode for editing ANTLR grammar files, i.e., files ending with `.g'.
;; ANTLR is ANother Tool for Language Recognition (an excellent alternative to
;; lex/yacc), see <http://www.ANTLR.org> and <news:comp.compilers.tools.pccts>.

;; This package provides the following features:
;;  * Syntax coloring (via font-lock) for grammar symbols and the code in
;;    actions.  The latter depends on the language settings.
;;  * Indentation for the current line (TAB) and selected region (C-M-\).
;;  * Support for imenu/speedbar: menu "Index" (Parser, Lexer, TreeParser).
;;  * Commands to move to previous/next rule, beginning/end of rule body etc.
;;  * Commands to hide/unhide actions, upcase/downcase literals.
;;  * Run ANTLR from within Emacs, create Makefile dependencies.

;; LANGUAGE SETTINGS.  This mode needs to know which language is used in
;; actions and semantic predicated of the grammar.  This information is used
;; for syntax coloring and the creation of the Makefile dependencies.  It is
;; stored in variable `antlr-language' and automatically set according to
;; ANTLR's file option "language", see `antlr-language-alist'.  The supported
;; languages are "Java" (java-mode) and "Cpp" (c++-mode).

;; INDENTATION.  This package supports ANTLR's (intended) indentation style
;; which is based on a simple paren/brace/bracket depth-level calculation, see
;; `antlr-indent-line'.  The indentation engine of cc-mode is only used inside
;; block comments (it is not easy to use it for actions, esp if they come early
;; in the rule body).  By default, this package defines a tab width of 4 to be
;; consistent to both ANTLR's conventions (TABs usage) and the
;; `c-indentation-style' "java" which sets `c-basic-offset' to 4, see
;; `antlr-tab-offset-alist'.  You might want to set this variable to nil.

;; SYNTAX COLORING comes in three phases.  First, comments and strings are
;; highlighted.  Second, the grammar code is highlighted according to
;; `antlr-font-lock-additional-keywords' (rule refs: dark blue, token refs:
;; dark orange, definition: bold blue).  Third, actions, semantic predicates
;; and arguments are highlighted according to the usual font-lock keywords of
;; `antlr-language', see also `antlr-font-lock-maximum-decoration'.  We define
;; special font-lock faces for the grammar code to allow you to distinguish
;; ANTLR keywords from Java/C++ keywords.

;; MAKEFILE CREATION.  Command \\[antlr-show-makefile-rules] shows/inserts the
;; dependencies for all grammar files in the current directory.  It considers
;; import/export vocabularies and grammar inheritance and provides a value for
;; the -glib option if necessary (which you have to edit if the super-grammar
;; is not in the same directory).

;; TODO.  Support to insert/change file/grammar/rule/subrule options.  imenu
;; support for method definitions in actions is not really planned (you can
;; send be a patch, though).  This mode would become too dependent on cc-mode
;; or I would have to do a lot of language-dependent things myself...

;; Bug fixes, bug reports, improvements, and suggestions are strongly
;; appreciated.  Please check the newest version first:
;;   http://www.fmi.uni-passau.de/~wedler/antlr-mode/changes.html

;;; Installation:

;; This file requires Emacs-20.3, XEmacs-20.4 or higher and package cc-mode.

;; If antlr-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;;   (autoload 'antlr-mode "antlr-mode" nil t)
;;   (setq auto-mode-alist (cons '("\\.g\\'" . antlr-mode) auto-mode-alist))
;;   (add-hook 'speedbar-load-hook  ; would be too late in antlr-mode.el
;;	       (lambda () (speedbar-add-supported-extension ".g")))

;; If you edit ANTLR's source files, you might also want to use
;;   (autoload 'antlr-set-tabs "antlr-mode")
;;   (add-hook 'java-mode-hook 'antlr-set-tabs)

;; I strongly recommend to use font-lock with a support mode like fast-lock,
;; lazy-lock or better jit-lock (Emacs-21.1+) / lazy-shot (XEmacs).

;; To customize, use `M-x customize-group RET antlr RET' or the custom browser
;; (Emacs->Programming->Languages->Antlr).

;;; Code:

(provide 'antlr-mode)
(eval-when-compile			; required and optional libraries
  (require 'cc-mode)
  (defvar outline-level) (defvar imenu-use-markers)
  (defvar imenu-create-index-function))
(eval-when-compile			; Emacs: cl, easymenu
  (require 'cl)
  (require 'easymenu))
(eval-when-compile			; XEmacs: Emacs vars
  (defvar inhibit-point-motion-hooks) (defvar deactivate-mark))

(eval-and-compile			; XEmacs functions, simplified
  (if (featurep 'xemacs)
      (defalias 'antlr-scan-sexps 'scan-sexps)
    (defalias 'antlr-scan-sexps 'antlr-scan-sexps-internal))
  (if (fboundp 'default-directory)
      (defalias 'antlr-default-directory 'default-directory)
    (defun antlr-default-directory () default-directory))
  (if (fboundp 'read-shell-command)
      (defalias 'antlr-read-shell-command 'read-shell-command)
    (defun antlr-read-shell-command (prompt &optional initial-input history)
      (read-from-minibuffer prompt initial-input nil nil
			    (or history 'shell-command-history))))
  (if (fboundp 'with-displaying-help-buffer)
      (defalias 'antlr-with-displaying-help-buffer 'with-displaying-help-buffer)
    (defun antlr-with-displaying-help-buffer (thunk &optional name)
      (with-output-to-temp-buffer "*Help*"
	(save-excursion (funcall thunk)))))
  (if (and (fboundp 'buffer-syntactic-context)
	   (fboundp 'buffer-syntactic-context-depth))
      (progn
	(defalias 'antlr-invalidate-context-cache 'antlr-xemacs-bug-workaround)
	(defalias 'antlr-syntactic-context 'antlr-fast-syntactic-context))
    (defalias 'antlr-invalidate-context-cache 'ignore)
    (defalias 'antlr-syntactic-context 'antlr-slow-syntactic-context)))



;;;;##########################################################################
;;;;  Variables
;;;;##########################################################################


(defgroup antlr nil
  "Major mode for ANTLR grammar files."
  :group 'languages
  :link '(emacs-commentary-link "antlr-mode.el")
  :link '(url-link "http://www.fmi.uni-passau.de/~wedler/antlr-mode/")
  :prefix "antlr-")

(defconst antlr-version "1.4"
  "ANTLR major mode version number.")


;;;===========================================================================
;;;  Controlling ANTLR's code generator (language option)
;;;===========================================================================

(defvar antlr-language nil
  "Major mode corresponding to ANTLR's \"language\" option.
Set via `antlr-language-alist'.  The only useful place to change this
buffer-local variable yourself is in `antlr-mode-hook' or in the \"local
variable list\" near the end of the file, see
`enable-local-variables'.")

(defcustom antlr-language-alist
  '((java-mode "Java" nil "\"Java\"" "Java")
    (c++-mode "C++" "\"Cpp\"" "Cpp"))
  "List of ANTLR's supported languages.
Each element in this list looks like
  (MAJOR-MODE MODELINE-STRING OPTION-VALUE...)

MAJOR-MODE, the major mode of the code in the grammar's actions, is the
value of `antlr-language' if the first group in the string matched by
REGEXP in `antlr-language-limit-n-regexp' is one of the OPTION-VALUEs.
An OPTION-VALUE of nil denotes the fallback element.  MODELINE-STRING is
also displayed in the modeline next to \"Antlr\"."
  :group 'antlr
  :type '(repeat (group :value (java-mode "")
			(function :tag "Major mode")
			(string :tag "Modeline string")
			(repeat :tag "ANTLR language option" :inline t
				(choice (const :tag "Default" nil)
					string )))))

(defcustom antlr-language-limit-n-regexp
  '(3000 . "language[ \t]*=[ \t]*\\(\"?[A-Z][A-Za-z_]*\"?\\)")
  "Used to set a reasonable value for `antlr-language'.
Looks like (LIMIT . REGEXP).  Search for REGEXP from the beginning of
the buffer to LIMIT and use the first group in the matched string to set
the language according to `antlr-language-alist'."
  :group 'antlr
  :type '(cons (choice :tag "Limit" (const :tag "No" nil) (integer :value 0))
	       regexp))


;;;===========================================================================
;;;  Hide/Unhide, Indent/Tabs
;;;===========================================================================

(defcustom antlr-action-visibility 3
  "Visibility of actions when command `antlr-hide-actions' is used.
If nil, the actions with their surrounding braces are hidden.  If a
number, do not hide the braces, only hide the contents if its length is
greater than this number."
  :group 'antlr
  :type '(choice (const :tag "Completely hidden" nil)
		 (integer :tag "Hidden if longer than" :value 3)))

(defcustom antlr-indent-comment 'tab
  "*Non-nil, if the indentation should touch lines in block comments.
If nil, no continuation line of a block comment is changed.  If t, they
are changed according to `c-indentation-line'.  When not nil and not t,
they are only changed by \\[antlr-indent-command]."
  :group 'antlr
  :type '(radio (const :tag "No" nil)
		(const :tag "Always" t)
		(sexp :tag "With TAB" :format "%t" :value tab)))

(defcustom antlr-tab-offset-alist
  '((antlr-mode nil 4 nil)
    (java-mode "antlr" 4 nil))
  "Alist to determine whether to use ANTLR's convention for TABs.
Each element looks like (MAJOR-MODE REGEXP TAB-WIDTH INDENT-TABS-MODE).
The first element whose MAJOR-MODE is nil or equal to `major-mode' and
whose REGEXP is nil or matches `buffer-file-name' is used to set
`tab-width' and `indent-tabs-mode'.  This is useful to support both
ANTLR's and Java's indentation styles.  Used by `antlr-set-tabs'."
  :group 'antlr
  :type '(repeat (group :value (antlr-mode nil 8 nil)
			(choice (const :tag "All" nil)
				(function :tag "Major mode"))
			(choice (const :tag "All" nil) regexp)
			(integer :tag "Tab width")
			(boolean :tag "Indent-tabs-mode"))))

(defvar antlr-indent-item-regexp
  "[]}):;|&]\\|default[ \t]*:\\|case[ \t]+\\('\\\\?.'\\|[0-9]+\\|[A-Za-z_][A-Za-z_0-9]*\\)[ \t]*:" ; & is local ANTLR extension
  "Regexp matching lines which should be indented by one TAB less.
See command \\[antlr-indent-command].")


;;;===========================================================================
;;;  Run tool, create Makefile dependencies
;;;===========================================================================

(defcustom antlr-tool-command "java antlr.Tool"
  "*Command used in \\[antlr-run-tool] to run the Antlr tool.
This variable should include all options passed to Antlr except the
option \"-glib\" which is automatically suggested if necessary."
  :group 'antlr
  :type 'string)

(defcustom antlr-ask-about-save t
  "*If not nil, \\[antlr-run-tool] asks which buffers to save.
Otherwise, it saves all modified buffers before running without asking."
  :group 'antlr
  :type 'boolean)

(defcustom antlr-makefile-specification
  '("\n" ("GENS" "GENS%d" " \\\n\t") "$(ANTLR)")
  "*Variable to specify the appearance of the generated makefile rules.
This variable influences the output of \\[antlr-show-makefile-rules].
It looks like (RULE-SEP GEN-VAR-SPEC COMMAND).

RULE-SEP is the string to separate different makefile rules.  COMMAND is
a string with the command which runs the Antlr tool, it should include
all options except the option \"-glib\" which is automatically added
if necessary.

If GEN-VAR-SPEC is nil, each target directly consists of a list of
files.  If GEN-VAR-SPEC looks like (GEN-VAR GEN-VAR-FORMAT GEN-SEP), a
Makefile variable is created for each rule target.

Then, GEN-VAR is a string with the name of the variable which contains
the file names of all makefile rules.  GEN-VAR-FORMAT is a format string
producing the variable of each target with substitution COUNT/%d where
COUNT starts with 1.  GEN-SEP is used to separate long variable values."
  :group 'antlr
  :type '(list (string :tag "Rule separator")
	       (choice
		(const :tag "Direct targets" nil)
		(list :tag "Variables for targets"
		      (string :tag "Variable for all targets")
		      (string :tag "Format for each target variable")
		      (string :tag "Variable separator")))
	       (string :tag "ANTLR command")))

(defvar antlr-file-formats-alist
  '((java-mode ("%sTokenTypes.java") ("%s.java"))
    (c++-mode ("%sTokenTypes.hpp") ("%s.cpp" "%s.hpp")))
  "Language dependent formats which specify generated files.
Each element in this list looks looks like
  (MAJOR-MODE (VOCAB-FILE-FORMAT...) (CLASS-FILE-FORMAT...)).

The element whose MAJOR-MODE is equal to `antlr-language' is used to
specify the generated files which are language dependent.  See variable
`antlr-special-file-formats' for language independent files.

VOCAB-FILE-FORMAT is a format string, it specifies with substitution
VOCAB/%s the generated file for each export vocabulary VOCAB.
CLASS-FILE-FORMAT is a format string, it specifies with substitution
CLASS/%s the generated file for each grammar class CLASS.")

(defvar antlr-special-file-formats '("%sTokenTypes.txt" "expanded%s.g")
  "Language independent formats which specify generated files.
The value looks like (VOCAB-FILE-FORMAT EXPANDED-GRAMMAR-FORMAT).

VOCAB-FILE-FORMAT is a format string, it specifies with substitution
VOCAB/%s the generated or input file for each export or import
vocabulary VOCAB, respectively.  EXPANDED-GRAMMAR-FORMAT is a format
string, it specifies with substitution GRAMMAR/%s the constructed
grammar file if the file GRAMMAR.g contains a grammar class which
extends a class other than \"Lexer\", \"Parser\" or \"TreeParser\".

See variable `antlr-file-formats-alist' for language dependent
formats.")

(defvar antlr-unknown-file-formats '("?%s?.g" "?%s?")
  "*Formats which specify the names of unknown files.
The value looks like (SUPER-GRAMMAR-FILE-FORMAT SUPER-EVOCAB-FORMAT).

SUPER-GRAMMAR-FORMAT is a format string, it specifies with substitution
SUPER/%s the name of a grammar file for Antlr's option \"-glib\" if no
grammar file in the current directory defines the class SUPER or if it
is defined more than once.  SUPER-EVOCAB-FORMAT is a format string, it
specifies with substitution SUPER/%s the name for the export vocabulary
of above mentioned class SUPER.")

(defvar antlr-help-unknown-file-text
  "## The following rules contain filenames of the form
##  \"?SUPERCLASS?.g\" (and \"?SUPERCLASS?TokenTypes.txt\")
## where SUPERCLASS is not found to be defined in any grammar file of
## the current directory or is defined more than once.  Please replace
## these filenames by the grammar files (and their exportVocab).\n\n"
  "String indicating the existence of unknown files in the Makefile.
See \\[antlr-show-makefile-rules] and `antlr-unknown-file-formats'.")

(defvar antlr-help-rules-intro
  "The following Makefile rules define the dependencies for all (non-
expanded) grammars in directory \"%s\".\n
They are stored in the kill-ring, i.e., you can insert them with C-y
into your Makefile.  You can also invoke M-x antlr-show-makefile-rules
from within a Makefile to insert them directly.\n\n\n"
  "Introduction to use with \\[antlr-show-makefile-rules].
It is a format string and used with substitution DIRECTORY/%s where
DIRECTORY is the name of the current directory.")


;;;===========================================================================
;;;  Menu
;;;===========================================================================

(defcustom antlr-imenu-name t
  "*Non-nil, if a \"Index\" menu should be added to the menubar.
If it is a string, it is used instead \"Index\".  Requires package
imenu."
  :group 'antlr
  :type '(choice (const :tag "No menu" nil)
		 (const :tag "Index menu" t)
		 (string :tag "Other menu name")))

(defvar antlr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'antlr-indent-command)
    (define-key map "\e\C-a" 'antlr-beginning-of-rule)
    (define-key map "\e\C-e" 'antlr-end-of-rule)
    (define-key map "\C-c\C-a" 'antlr-beginning-of-body)
    (define-key map "\C-c\C-e" 'antlr-end-of-body)
    (define-key map "\C-c\C-f" 'c-forward-into-nomenclature)
    (define-key map "\C-c\C-b" 'c-backward-into-nomenclature)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "\C-c\C-v" 'antlr-hide-actions)
    (define-key map "\C-c\C-r" 'antlr-run-tool)
    ;; I'm too lazy to define my own:
    (define-key map "\ea" 'c-beginning-of-statement)
    (define-key map "\ee" 'c-end-of-statement)
    map)
  "Keymap used in `antlr-mode' buffers.")

(easy-menu-define antlr-mode-menu
		  antlr-mode-map
		  "Major mode menu."
		  '("Antlr"
		    ["Indent Line" antlr-indent-command
		     :active (not buffer-read-only)]
		    ["Indent for Comment" indent-for-comment
		     :active (not buffer-read-only)]
		    ["Comment Out Region" comment-region
		     :active (and (not buffer-read-only)
				  (c-region-is-active-p))]
		    ["Uncomment Region"
		     (comment-region (region-beginning) (region-end) '(4))
		     :active (and (not buffer-read-only)
				  (c-region-is-active-p))]
		    "---"
		    ["Backward Rule" antlr-beginning-of-rule t]
		    ["Forward Rule" antlr-end-of-rule t]
		    ["Start of Rule Body" antlr-beginning-of-body
		     :active (antlr-inside-rule-p)]
		    ["End of Rule Body" antlr-end-of-body
		     :active (antlr-inside-rule-p)]
		    "---"
		    ["Backward Statement" c-beginning-of-statement t]
		    ["Forward Statement" c-end-of-statement t]
		    ["Backward Into Nomencl." c-backward-into-nomenclature t]
		    ["Forward Into Nomencl." c-forward-into-nomenclature t]
		    "---"
		    ["Hide Actions (incl. Args)" antlr-hide-actions t]
		    ["Hide Actions (excl. Args)" (antlr-hide-actions 2) t]
		    ["Unhide All Actions" (antlr-hide-actions 0) t]
		    "---"
		    ["Run Tool on Grammar" antlr-run-tool t]
		    ["Show Makefile Rules" antlr-show-makefile-rules t]))


;;;===========================================================================
;;;  font-lock
;;;===========================================================================

(defcustom antlr-font-lock-maximum-decoration 'inherit
  "*The maximum decoration level for fontifying actions.
Value `none' means, do not fontify actions, just normal grammar code
according to `antlr-font-lock-additional-keywords'.  Value `inherit'
means, use value of `font-lock-maximum-decoration'.  Any other value is
interpreted as in `font-lock-maximum-decoration' with no level-0
fontification, see `antlr-font-lock-keywords-alist'.

While calculating the decoration level for actions, `major-mode' is
bound to `antlr-language'.  For example, with value
  ((java-mode . 2) (c++-mode . 0))
Java actions are fontified with level 2 and C++ actions are not
fontified at all."
  :type '(choice (const :tag "none" none)
		 (const :tag "inherit" inherit)
		 (const :tag "default" nil)
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
				      (integer :tag "level" 1))))))

(defconst antlr-no-action-keywords nil
  ;; Using nil directly won't work (would use highest level, see
  ;; `font-lock-choose-keywords'), but a non-symbol, i.e., (list), at `car'
  ;; would break Emacs-21.0:
  "Empty font-lock keywords for actions.
Do not change the value of this constant.")

(defvar antlr-font-lock-keywords-alist
  '((java-mode
     antlr-no-action-keywords
     java-font-lock-keywords-1 java-font-lock-keywords-2
     java-font-lock-keywords-3)
    (c++-mode
     antlr-no-action-keywords
     c++-font-lock-keywords-1 c++-font-lock-keywords-2
     c++-font-lock-keywords-3))
  "List of font-lock keywords for actions in the grammar.
Each element in this list looks like
  (MAJOR-MODE KEYWORD...)

If `antlr-language' is equal to MAJOR-MODE, the KEYWORDs are the
font-lock keywords according to `font-lock-defaults' used for the code
in the grammar's actions and semantic predicates, see
`antlr-font-lock-maximum-decoration'.")

(defvar antlr-font-lock-keyword-face 'antlr-font-lock-keyword-face)
(defface antlr-font-lock-keyword-face
  '((((class color) (background light)) (:foreground "black" :bold t)))
  "ANTLR keywords."
  :group 'antlr)

(defvar antlr-font-lock-ruledef-face 'antlr-font-lock-ruledef-face)
(defface antlr-font-lock-ruledef-face
  '((((class color) (background light)) (:foreground "blue" :bold t)))
  "ANTLR rule references (definition)."
  :group 'antlr)

(defvar antlr-font-lock-tokendef-face 'antlr-font-lock-tokendef-face)
(defface antlr-font-lock-tokendef-face
  '((((class color) (background light)) (:foreground "blue" :bold t)))
  "ANTLR token references (definition)."
  :group 'antlr)

(defvar antlr-font-lock-ruleref-face 'antlr-font-lock-ruleref-face)
(defface antlr-font-lock-ruleref-face
  '((((class color) (background light)) (:foreground "blue4")))
  "ANTLR rule references (usage)."
  :group 'antlr)

(defvar antlr-font-lock-tokenref-face 'antlr-font-lock-tokenref-face)
(defface antlr-font-lock-tokenref-face
  '((((class color) (background light)) (:foreground "orange4")))
  "ANTLR token references (usage)."
  :group 'antlr)

(defvar antlr-font-lock-literal-face 'antlr-font-lock-literal-face)
(defface antlr-font-lock-literal-face
  '((((class color) (background light)) (:foreground "brown4" :bold t)))
  "ANTLR literal tokens consisting merely of letter-like characters."
  :group 'antlr)

(defvar antlr-font-lock-additional-keywords
  `((antlr-invalidate-context-cache)
    ("\\$setType[ \t]*(\\([A-Za-z\300-\326\330-\337]\\sw*\\))"
     (1 antlr-font-lock-tokendef-face))
    ("\\$\\sw+" (0 font-lock-keyword-face))
    ;; the tokens are already fontified as string/docstrings:
    (,(lambda (limit)
	(antlr-re-search-forward "\"\\(\\sw\\(\\sw\\|-\\)*\\)\"" limit))
     (1 antlr-font-lock-literal-face t)
     ,@(and (string-match "XEmacs" emacs-version)
	    '((0 nil))))		; XEmacs bug workaround
    (,(lambda (limit)
	(antlr-re-search-forward
	 "^\\(class\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)[ \t]+\\(extends\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)[ \t]*;" limit))
     (1 antlr-font-lock-keyword-face)
     (2 antlr-font-lock-ruledef-face)
     (3 antlr-font-lock-keyword-face)
     (4 (if (member (match-string 4) '("Lexer" "Parser" "TreeParser"))
	    'antlr-font-lock-keyword-face
	  'font-lock-type-face)))
    (,(lambda (limit)
	(antlr-re-search-forward
	 "\\<\\(header\\|options\\|tokens\\|exception\\|catch\\|returns\\)\\>"
	 limit))
     (1 antlr-font-lock-keyword-face))
    (,(lambda (limit)
	(antlr-re-search-forward
	 "^\\(private\\|public\\|protected\\)\\>\\([ \t]+\\(\\sw+\\)\\)?"
	 limit))
     (1 font-lock-type-face)		; not XEmacs' java level-3 fruit salad
     (3 (if (antlr-upcase-p (char-after (match-beginning 3)))
	    'antlr-font-lock-tokendef-face
	  'antlr-font-lock-ruledef-face) nil t))
    (,(lambda (limit)
	(antlr-re-search-forward "^\\sw+" limit))
     (0 (if (antlr-upcase-p (char-after (match-beginning 0)))
	    'antlr-font-lock-tokendef-face
	  'antlr-font-lock-ruledef-face) nil t))
    (,(lambda (limit)
	;; not only before a rule ref, also before a literal
	(antlr-re-search-forward "\\<\\(\\sw+\\)[ \t]*:" limit))
     (1 font-lock-variable-name-face))
    (,(lambda (limit)
	(antlr-re-search-forward "\\<\\(\\sw+[ \t]*=[ \t]*\\)?\\(\\sw+[ \t]*:[ \t]*\\)?\\(\\sw+\\)" limit))
     ;;(1 antlr-font-lock-default-face nil t) ; fool java-font-lock-keywords
     (3 (if (antlr-upcase-p (char-after (match-beginning 3)))
	    'antlr-font-lock-tokenref-face
	  'antlr-font-lock-ruleref-face))))
  "Font-lock keywords for ANTLR's normal grammar code.
See `antlr-font-lock-keywords-alist' for the keywords of actions.")

(defvar antlr-font-lock-defaults
  '(antlr-font-lock-keywords
    nil nil ((?_ . "w") (?\( . ".") (?\) . ".")) beginning-of-defun)
  "Font-lock defaults used for ANTLR syntax coloring.
The SYNTAX-ALIST element is also used to initialize
`antlr-action-syntax-table'.")


;;;===========================================================================
;;;  Internal variables
;;;===========================================================================

(defvar antlr-mode-hook nil
  "Hook called by `antlr-mode'.")

(defvar antlr-mode-syntax-table nil
  "Syntax table used in `antlr-mode' buffers.
If non-nil, it will be initialized in `antlr-mode'.")

;; used for "in Java/C++ code" = syntactic-depth>0
(defvar antlr-action-syntax-table nil
  "Syntax table used for ANTLR action parsing.
Initialized by `antlr-mode-syntax-table', changed by SYNTAX-ALIST in
`antlr-font-lock-defaults'.  This table should be selected if you use
`buffer-syntactic-context' and `buffer-syntactic-context-depth' in order
not to confuse their context_cache.")

(defvar antlr-mode-abbrev-table nil
  "Abbreviation table used in `antlr-mode' buffers.")
(define-abbrev-table 'antlr-mode-abbrev-table ())



;;;;##########################################################################
;;;;  The Code
;;;;##########################################################################


;;;===========================================================================
;;;  Syntax functions -- Emacs vs XEmacs dependent
;;;===========================================================================

;; From help.el (XEmacs-21.1), without `copy-syntax-table'
(defmacro antlr-with-syntax-table (syntab &rest body)
  "Evaluate BODY with the syntax table SYNTAB."
  `(let ((stab (syntax-table)))
     (unwind-protect
	 (progn (set-syntax-table ,syntab) ,@body)
       (set-syntax-table stab))))
(put 'antlr-with-syntax-table 'lisp-indent-function 1)
(put 'antlr-with-syntax-table 'edebug-form-spec '(form body))

(defun antlr-scan-sexps-internal (from count &optional dummy no-error)
;; checkdoc-params: (from count dummy)
  "Like `scan-sexps' but with additional arguments.
When optional arg NO-ERROR is non-nil, `scan-sexps' will return nil
instead of signaling an error."
  (if no-error
      (condition-case nil
	  (scan-sexps from count)
	(t nil))
    (scan-sexps from count)))

(defun antlr-xemacs-bug-workaround (&rest dummies)
;; checkdoc-params: (dummies)
  "Invalidate context_cache for syntactical context information."
  ;; XEmacs bug workaround
  (save-excursion
    (set-buffer (get-buffer-create " ANTLR XEmacs bug workaround"))
    (buffer-syntactic-context-depth))
  nil)

(defun antlr-fast-syntactic-context ()
  "Return some syntactic context information.
Return `string' if point is within a string, `block-comment' or
`comment' is point is within a comment or the depth within all
parenthesis-syntax delimiters at point otherwise.
WARNING: this may alter `match-data'."
  (or (buffer-syntactic-context) (buffer-syntactic-context-depth)))

(defun antlr-slow-syntactic-context ()
  "Return some syntactic context information.
Return `string' if point is within a string, `block-comment' or
`comment' is point is within a comment or the depth within all
parenthesis-syntax delimiters at point otherwise.
WARNING: this may alter `match-data'."
  (let ((orig (point)))
    (beginning-of-defun)
    (let ((state (parse-partial-sexp (point) orig)))
      (goto-char orig)
      (cond ((nth 3 state) 'string)
	    ((nth 4 state) 'comment)	; block-comment? -- we don't care
	    (t (car state))))))


;;;===========================================================================
;;;  Misc functions
;;;===========================================================================

(defun antlr-upcase-p (char)
  "Non-nil, if CHAR is an uppercase character (if CHAR was a char)."
  ;; in XEmacs, upcase only works for ASCII
  (or (and (<= ?A char) (<= char ?Z))
      (and (<= ?\300 char) (<= char ?\337)))) ; ?\327 is no letter

(defun antlr-re-search-forward (regexp bound)
  "Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.  Return
nil if no occurrence was found.  Do not search within comments, strings
and actions/semantic predicates.  BOUND bounds the search; it is a
buffer position.  See also the functions `match-beginning', `match-end'
and `replace-match'."
  ;; WARNING: Should only be used with `antlr-action-syntax-table'!
  (let ((continue t))
    (while (and (re-search-forward regexp bound 'limit)
		(save-match-data
		  (if (eq (antlr-syntactic-context) 0)
		      (setq continue nil)
		    t))))
    (if continue nil (point))))

(defun antlr-search-forward (string)
  "Search forward from point for STRING.
Set point to the end of the occurrence found, and return point.  Return
nil if no occurrence was found.  Do not search within comments, strings
and actions/semantic predicates."
  ;; WARNING: Should only be used with `antlr-action-syntax-table'!
  (let ((continue t))
    (while (and (search-forward string nil 'limit)
		(if (eq (antlr-syntactic-context) 0) (setq continue nil) t)))
    (if continue nil (point))))

(defun antlr-search-backward (string)
  "Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
Return nil if no occurrence was found.  Do not search within comments,
strings and actions/semantic predicates."
  ;; WARNING: Should only be used with `antlr-action-syntax-table'!
  (let ((continue t))
    (while (and (search-backward string nil 'limit)
		(if (eq (antlr-syntactic-context) 0) (setq continue nil) t)))
    (if continue nil (point))))

(defsubst antlr-skip-sexps (count)
  "Skip the next COUNT balanced expressions and the comments after it.
Return position before the comments after the last expression."
  (goto-char (or (antlr-scan-sexps (point) count nil t) (point-max)))
  (prog1 (point)
    (c-forward-syntactic-ws)))


;;;===========================================================================
;;;  font-lock
;;;===========================================================================

(defun antlr-font-lock-keywords ()
  "Return font-lock keywords for current buffer.
See `antlr-font-lock-additional-keywords', `antlr-language' and
`antlr-font-lock-maximum-decoration'."
  (if (eq antlr-font-lock-maximum-decoration 'none)
      antlr-font-lock-additional-keywords
    (append antlr-font-lock-additional-keywords
	    (eval (let ((major-mode antlr-language)) ; dynamic
			(font-lock-choose-keywords
			 (cdr (assq antlr-language
				    antlr-font-lock-keywords-alist))
			 (if (eq antlr-font-lock-maximum-decoration 'inherit)
			     font-lock-maximum-decoration
			   antlr-font-lock-maximum-decoration)))))))


;;;===========================================================================
;;;  imenu support
;;;===========================================================================

(defun antlr-imenu-create-index-function ()
  "Return imenu index-alist for ANTLR grammar files."
  (let ((items nil)
	(classes nil)
	(semi (point-max)))
    ;; Using `imenu-progress-message' would require imenu for compilation --
    ;; nobody is missing these messages...
    (antlr-with-syntax-table antlr-action-syntax-table
      ;; We stick to the imenu standard and search backwards, although I don't
      ;; think this is right.  It is slower and more likely not to work during
      ;; editing (you are more likely to add functions to the end of the file).
      (while semi
	(goto-char semi)
	(if (setq semi (antlr-search-backward ";"))
	    (progn (forward-char) (antlr-skip-exception-part t))
	  (antlr-skip-file-prelude t))
	(if (looking-at "{") (antlr-skip-sexps 1))
	(if (looking-at "class[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)[ \t]+extends[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)[ \t]*;")
	    (push (cons (match-string 1)
			(if imenu-use-markers
			    (copy-marker (match-beginning 1))
			  (match-beginning 1)))
		  classes)
	  (if (looking-at "p\\(ublic\\|rotected\\|rivate\\)")
	      (antlr-skip-sexps 1))
	  (when (looking-at "\\sw+")
	    (push (cons (match-string 0)
			(if imenu-use-markers
			    (copy-marker (match-beginning 0))
			  (match-beginning 0)))
		  items)))))
    (if classes (cons (cons "Classes" classes) items) items)))


;;;===========================================================================
;;;  Parse grammar files (internal functions)
;;;===========================================================================

(defun antlr-skip-exception-part (skip-comment)
  "Skip exception part of current rule, i.e., everything after `;'.
This also includes the options and tokens part of a grammar class
header.  If SKIP-COMMENT is non-nil, also skip the comment after that
part."
  (let ((pos (point))
	(class nil))
    (c-forward-syntactic-ws)
    (while (looking-at "options\\>\\|tokens\\>")
      (setq class t)
      (setq pos (antlr-skip-sexps 2)))
    (if class
	;; Problem: an action only belongs to a class def, not a normal rule.
	;; But checking the current rule type is too expensive => only expect
	;; an action if we have found an option or tokens part.
	(if (looking-at "{") (setq pos (antlr-skip-sexps 1)))
      (while (looking-at "exception\\>")
	(setq pos (antlr-skip-sexps 1))
	(if (looking-at "\\[") (setq pos (antlr-skip-sexps 1)))
	(while (looking-at "catch\\>")
	  (setq pos (antlr-skip-sexps 3)))))
    (or skip-comment (goto-char pos))))

(defun antlr-skip-file-prelude (skip-comment)
  "Skip the file prelude: the header and file options.
If SKIP-COMMENT is non-nil, also skip the comment after that part.
Return the start position of the file prelude."
  (let* ((pos (point))
	 (pos0 pos))
    (c-forward-syntactic-ws)
    (if skip-comment (setq pos0 (point)))
    (while (looking-at "header\\>[ \t]*\\(\"\\)?")
      (setq pos (antlr-skip-sexps (if (match-beginning 1) 3 2))))
    (if (looking-at "options\\>") (setq pos (antlr-skip-sexps 2)))
    (or skip-comment (goto-char pos))
    pos0))

(defun antlr-next-rule (arg skip-comment)
  "Move forward to next end of rule.  Do it ARG many times.
A grammar class header and the file prelude are also considered as a
rule.  Negative argument ARG means move back to ARGth preceding end of
rule.  The behavior is not defined when ARG is zero.  If SKIP-COMMENT
is non-nil, move to beginning of the rule."
  ;; WARNING: Should only be used with `antlr-action-syntax-table'!
  ;; PRE: ARG<>0
  (let ((pos (point))
	(beg (point)))
    ;; first look whether point is in exception part
    (if (antlr-search-backward ";")
	(progn
	  (setq beg (point))
	  (forward-char)
	  (antlr-skip-exception-part skip-comment))
      (antlr-skip-file-prelude skip-comment))
    (if (< arg 0)
	(unless (and (< (point) pos) (zerop (incf arg)))
	  ;; if we have moved backward, we already moved one defun backward
	  (goto-char beg)		; rewind (to ";" / point)
	  (while (and arg (<= (incf arg) 0))
	    (if (antlr-search-backward ";")
		(setq beg (point))
	      (when (>= arg -1)
		;; try file prelude:
		(setq pos (antlr-skip-file-prelude skip-comment))
		(if (zerop arg)
		    (if (>= (point) beg)
			(goto-char (if (>= pos beg) (point-min) pos)))
		  (goto-char (if (or (>= (point) beg) (= (point) pos))
				 (point-min) pos))))
	      (setq arg nil)))
	  (when arg			; always found a ";"
	    (forward-char)
	    (antlr-skip-exception-part skip-comment)))
      (if (<= (point) pos)		; moved backward?
	  (goto-char pos)		; rewind
	(decf arg))			; already moved one defun forward
      (unless (zerop arg)
	(while (>= (decf arg) 0)
	  (antlr-search-forward ";"))
	(antlr-skip-exception-part skip-comment)))))

(defun antlr-outside-rule-p ()
  "Non-nil if point is outside a grammar rule.
Move to the beginning of the current rule if point is inside a rule."
  ;; WARNING: Should only be used with `antlr-action-syntax-table'!
  (let ((pos (point)))
    (antlr-next-rule -1 nil)
    (let ((between (or (bobp) (< (point) pos))))
      (c-forward-syntactic-ws)
      (and between (> (point) pos) (goto-char pos)))))


;;;===========================================================================
;;;  Parse grammar files (commands)
;;;===========================================================================
;; No (interactive "_") in Emacs... use `zmacs-region-stays'.

(defun antlr-inside-rule-p ()
  "Non-nil if point is inside a grammar rule.
A grammar class header and the file prelude are also considered as a
rule."
  (save-excursion
    (antlr-with-syntax-table antlr-action-syntax-table
      (not (antlr-outside-rule-p)))))

(defun antlr-end-of-rule (&optional arg)
  "Move forward to next end of rule.  Do it ARG [default: 1] many times.
A grammar class header and the file prelude are also considered as a
rule.  Negative argument ARG means move back to ARGth preceding end of
rule.  If ARG is zero, run `antlr-end-of-body'."
  (interactive "p")
  (if (zerop arg)
      (antlr-end-of-body)
    (antlr-with-syntax-table antlr-action-syntax-table
      (antlr-next-rule arg nil))
    (setq zmacs-region-stays t)))

(defun antlr-beginning-of-rule (&optional arg)
  "Move backward to preceding beginning of rule.  Do it ARG many times.
A grammar class header and the file prelude are also considered as a
rule.  Negative argument ARG means move forward to ARGth next beginning
of rule.  If ARG is zero, run `antlr-beginning-of-body'."
  (interactive "p")
  (if (zerop arg)
      (antlr-beginning-of-body)
    (antlr-with-syntax-table antlr-action-syntax-table
      (antlr-next-rule (- arg) t))
    (setq zmacs-region-stays t)))

(defun antlr-end-of-body (&optional msg)
  "Move to position after the `;' of the current rule.
A grammar class header is also considered as a rule.  With optional
prefix arg MSG, move to `:'."
  (interactive)
  (antlr-with-syntax-table antlr-action-syntax-table
    (let ((orig (point)))
      (if (antlr-outside-rule-p)
	  (error "Outside an ANTLR rule"))
      (let ((bor (point)))
	(when (< (antlr-skip-file-prelude t) (point))
	  ;; Yes, we are in the file prelude
	  (goto-char orig)
	  (error (or msg "The file prelude is without `;'")))
	(antlr-search-forward ";")
	(when msg
	  (when (< (point)
		   (progn (goto-char bor)
			  (or (antlr-search-forward ":") (point-max))))
	    (goto-char orig)
	    (error msg))
	  (c-forward-syntactic-ws)))))
  (setq zmacs-region-stays t))

(defun antlr-beginning-of-body ()
  "Move to the first element after the `:' of the current rule."
  (interactive)
  (antlr-end-of-body "Class headers and the file prelude are without `:'"))


;;;===========================================================================
;;;  Literal normalization, Hide Actions
;;;===========================================================================

(defun antlr-downcase-literals (&optional transform)
  "Convert all literals in buffer to lower case.
If non-nil, TRANSFORM is used on literals instead of `downcase-region'."
  (interactive)
  (or transform (setq transform 'downcase-region))
  (let ((literals 0))
    (save-excursion
      (goto-char (point-min))
      (antlr-with-syntax-table antlr-action-syntax-table
	(antlr-invalidate-context-cache)
	(while (antlr-re-search-forward "\"\\(\\sw\\(\\sw\\|-\\)*\\)\"" nil)
	  (funcall transform (match-beginning 0) (match-end 0))
	  (incf literals))))
    (message "Transformed %d literals" literals)))

(defun antlr-upcase-literals ()
  "Convert all literals in buffer to upper case."
  (interactive)
  (antlr-downcase-literals 'upcase-region))

(defun antlr-hide-actions (arg &optional silent)
  "Hide or unhide all actions in buffer.
Hide all actions including arguments in brackets if ARG is 1 or if
called interactively without prefix argument.  Hide all actions
excluding arguments in brackets if ARG is 2 or higher.  Unhide all
actions if ARG is 0 or negative.  See `antlr-action-visibility'.

Display a message unless optional argument SILENT is non-nil."
  (interactive "p")
  ;; from Emacs/lazy-lock: `save-buffer-state'
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t) (inhibit-read-only t)
	(inhibit-point-motion-hooks t) deactivate-mark ; Emacs only
	before-change-functions after-change-functions
	buffer-file-name buffer-file-truename)
    (if (> arg 0)
	(let ((regexp (if (= arg 1) "[]}]" "}"))
	      (diff (and antlr-action-visibility
			 (+ (max antlr-action-visibility 0) 2))))
	  (antlr-hide-actions 0 t)
	  (save-excursion
	    (goto-char (point-min))
	    (antlr-with-syntax-table antlr-action-syntax-table
	      (antlr-invalidate-context-cache)
	      (while (antlr-re-search-forward regexp nil)
		(let ((beg (antlr-scan-sexps (point) -1 nil t)))
		  (when beg
		    (if diff		; braces are visible
			(if (> (point) (+ beg diff))
			    (add-text-properties (1+ beg) (1- (point))
						 '(invisible t intangible t)))
		      ;; if actions is on line(s) of its own, hide WS
		      (and (looking-at "[ \t]*$")
			   (save-excursion
			     (goto-char beg)
			     (skip-chars-backward " \t")
			     (and (bolp) (setq beg (point))))
			   (beginning-of-line 2)) ; beginning of next line
		      (add-text-properties beg (point)
					   '(invisible t intangible t))))))))
	  (or silent
	      (message "Hide all actions (%s arguments)...done"
		       (if (= arg 1) "including" "excluding"))))
      (remove-text-properties (point-min) (point-max)
			      '(invisible nil intangible nil))
      (or silent
	  (message "Unhide all actions (including arguments)...done")))
    (and (not modified) (buffer-modified-p)
	 (set-buffer-modified-p nil))))


;;;===========================================================================
;;;  Compute dependencies
;;;===========================================================================

(defun antlr-file-dependencies ()
  "Return dependencies for grammar in current buffer.
The result looks like (FILE (CLASSES . SUPERS) VOCABS . LANGUAGE)
where CLASSES = ((CLASS . CLASS-EVOCAB) ...),
      SUPERS  = ((SUPER . USE-EVOCAB-P) ...), and
      VOCABS  = ((EVOCAB ...) . (IVOCAB ...))

FILE is the current buffer's file-name without directory part and
LANGUAGE is the value of `antlr-language' in the current buffer.  Each
EVOCAB is an export vocabulary and each IVOCAB is an import vocabulary.

Each CLASS is a grammar class with its export vocabulary CLASS-EVOCAB.
Each SUPER is a super-grammar class where USE-EVOCAB-P indicates whether
its export vocabulary is used as an import vocabulary."
  (unless buffer-file-name
    (error "Grammar buffer does not visit a file"))
  (let (classes exportVocabs importVocabs superclasses default-vocab)
    (antlr-with-syntax-table antlr-action-syntax-table
      (goto-char (point-min))
      (while (antlr-re-search-forward "class[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)[ \t]+extends[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)[ \t]*;" nil)
	;; parse class definition --------------------------------------------
	(let* ((class (match-string 1))
	       (sclass (match-string 2))
	       ;; export vocab defaults to class name (first grammar in file)
	       ;; or to the export vocab of the first grammar in file:
	       (evocab (or default-vocab class))
	       (ivocab nil))
	  (goto-char (match-end 0))
	  (c-forward-syntactic-ws)
	  (while (looking-at "options\\>\\|\\(tokens\\)\\>")
	    (if (match-beginning 1)
		(antlr-skip-sexps 2)
	      (goto-char (match-end 0))
	      (c-forward-syntactic-ws)
	      ;; parse grammar option section --------------------------------
	      (when (eq (char-after (point)) ?\{)
		(let* ((beg (1+ (point)))
		       (end (1- (antlr-skip-sexps 1)))
		       (cont (point)))
		(goto-char beg)
		(if (re-search-forward "\\<exportVocab[ \t]*=[ \t]*\\([A-Za-z\300-\326\330-\337]\\sw*\\)" end t)
		    (setq evocab (match-string 1)))
		(goto-char beg)
		(if (re-search-forward "\\<importVocab[ \t]*=[ \t]*\\([A-Za-z\300-\326\330-\337]\\sw*\\)" end t)
		    (setq ivocab (match-string 1)))
		(goto-char cont)))))
	  (unless (member sclass '("Parser" "Lexer" "TreeParser"))
	    (let ((super (assoc sclass superclasses)))
	      (if super
		  (or ivocab (setcdr super t))
		(push (cons sclass (null ivocab)) superclasses))))
	  ;; remember class with export vocabulary:
	  (push (cons class evocab) classes)
	  ;; default export vocab is export vocab of first grammar in file:
	  (or default-vocab (setq default-vocab evocab))
	  (or (member evocab exportVocabs) (push evocab exportVocabs))
	  (or (null ivocab)
	      (member ivocab importVocabs) (push ivocab importVocabs)))))
    (if classes
	(list* (file-name-nondirectory buffer-file-name)
	       (cons (nreverse classes) (nreverse superclasses))
	       (cons (nreverse exportVocabs) (nreverse importVocabs))
	       antlr-language))))

(defun antlr-directory-dependencies (dirname)
  "Return dependencies for all grammar files in directory DIRNAME.
The result looks like ((CLASS-SPEC ...) . (FILE-DEP ...))
where CLASS-SPEC = (CLASS (FILE . EVOCAB) ...).

FILE-DEP are the dependencies for each grammar file in DIRNAME, see
`antlr-file-dependencies'.  For each grammar class CLASS, FILE is a
grammar file in which CLASS is defined and EVOCAB is the name of the
export vocabulary specified in that file."
  (let ((grammar (directory-files dirname t "\\.g\\'")))
    (when grammar
      (let ((temp-buffer (get-buffer-create
			  (generate-new-buffer-name " *temp*")))
	    (antlr-imenu-name nil)		; dynamic-let: no imenu
	    (expanded-regexp (concat (format (regexp-quote
					      (cadr antlr-special-file-formats))
					     ".+")
				     "\\'"))
	    classes dependencies)
	(unwind-protect
	    (save-excursion
	      (set-buffer temp-buffer)
	      (widen)			; just in case...
	      (dolist (file grammar)
		(when (and (file-regular-p file)
			   (null (string-match expanded-regexp file)))
		  (insert-file-contents file t nil nil t)
		  (normal-mode t)	; necessary for major-mode, syntax
					; table and `antlr-language'
		  (when (eq major-mode 'antlr-mode)
		    (let* ((file-deps (antlr-file-dependencies))
			   (file (car file-deps)))
		      (when file-deps
			(dolist (class-def (caadr file-deps))
			  (let ((file-evocab (cons file (cdr class-def)))
				(class-spec (assoc (car class-def) classes)))
			    (if class-spec
				(nconc (cdr class-spec) (list file-evocab))
			      (push (list (car class-def) file-evocab)
				    classes))))
			(push file-deps dependencies)))))))
	  (kill-buffer temp-buffer))
	(cons (nreverse classes) (nreverse dependencies))))))


;;;===========================================================================
;;;  Compilation: run ANTLR tool
;;;===========================================================================

(defun antlr-superclasses-glibs (supers classes)
  "Compute the grammar lib option for the super grammars SUPERS.
Look in CLASSES for the right grammar lib files for SUPERS.  SUPERS is
part SUPER in the result of `antlr-file-dependencies'.  CLASSES is the
part (CLASS-SPEC ...) in the result of `antlr-directory-dependencies'.

The result looks like (OPTION WITH-UNKNOWN GLIB ...).  OPTION is the
complete \"-glib\" option.  WITH-UNKNOWN has value t iff there is none
or more than one grammar file for at least one super grammar.

Each GLIB looks like (GRAMMAR-FILE . EVOCAB).  GRAMMAR-FILE is a file in
which a super-grammar is defined.  EVOCAB is the value of the export
vocabulary of the super-grammar or nil if it is not needed."
  ;; If the superclass is defined in the same file, that file will be included
  ;; with -glib again.  This will lead to a redefinition.  But defining a
  ;; analyzer of the same class twice in a file will lead to an error anyway...
  (let (glibs unknown)
    (while supers
      (let* ((super (pop supers))
	     (sup-files (cdr (assoc (car super) classes)))
	     (file (and sup-files (null (cdr sup-files)) (car sup-files))))
	(or file (setq unknown t))	; not exactly one file
	(push (cons (or (car file)
			(format (car antlr-unknown-file-formats)
				(car super)))
		    (and (cdr super)
			 (or (cdr file)
			     (format (cadr antlr-unknown-file-formats)
				     (car super)))))
	      glibs)))
    (cons (if glibs (concat " -glib " (mapconcat 'car glibs ";")) "")
	  (cons unknown glibs))))

(defun antlr-run-tool (command file &optional saved)
  "Run Antlr took COMMAND on grammar FILE.
When called interactively, COMMAND is read from the minibuffer and
defaults to `antlr-tool-command' with a computed \"-glib\" option if
necessary.

Save all buffers first unless optional value SAVED is non-nil.  When
called interactively, the buffers are always saved, see also variable
`antlr-ask-about-save'."
  (interactive
   ;; code in `interactive' is not compiled: do not use cl macros (`cdadr')
   (let* ((supers (cdr (cadr (save-excursion
			       (save-restriction
				 (widen)
				 (antlr-file-dependencies))))))
	  (glibs ""))
     (when supers
       (save-some-buffers (not antlr-ask-about-save) nil)
       (setq glibs (car (antlr-superclasses-glibs
			 supers
			 (car (antlr-directory-dependencies
			       (antlr-default-directory)))))))
     (list (antlr-read-shell-command "Run Antlr on current file with: "
				     (concat antlr-tool-command glibs " "))
	   buffer-file-name
	   supers)))
  (or saved (save-some-buffers (not antlr-ask-about-save)))
  (let ((default-directory (file-name-directory file)))
    (require 'compile)			; only `compile' autoload
    (compile-internal (concat command " " (file-name-nondirectory file))
		      "No more errors" "Antlr-Run")))


;;;===========================================================================
;;;  Makefile creation
;;;===========================================================================

(defun antlr-makefile-insert-variable (number pre post)
  "Insert Makefile variable numbered NUMBER according to specification.
Also insert strings PRE and POST before and after the variable."
  (let ((spec (cadr antlr-makefile-specification)))
    (when spec
      (insert pre
	      (if number (format (cadr spec) number) (car spec))
	      post))))

(defun antlr-insert-makefile-rules (&optional in-makefile)
  "Insert Makefile rules in the current buffer at point.
IN-MAKEFILE is non-nil, if the current buffer is the Makefile.  See
command `antlr-show-makefile-rules' for detail."
  (let* ((dirname (antlr-default-directory))
	 (deps0 (antlr-directory-dependencies dirname))
	 (classes (car deps0))		; CLASS -> (FILE . EVOCAB) ...
	 (deps (cdr deps0))		; FILE -> (c . s) (ev . iv) . LANGUAGE
	 (with-error nil)
	 (gen-sep (or (caddr (cadr antlr-makefile-specification)) " "))
	 (n (and (cdr deps) (cadr antlr-makefile-specification) 0)))
    (or in-makefile (set-buffer standard-output))
    (dolist (dep deps)
      (let ((supers (cdadr dep))
	    (lang (cdr (assoc (cdddr dep) antlr-file-formats-alist))))
	(if n (incf n))
	(antlr-makefile-insert-variable n "" " =")
	(if supers
	    (insert " "
		    (format (cadr antlr-special-file-formats)
			    (file-name-sans-extension (car dep)))))
	(dolist (class-def (caadr dep))
	  (let ((sep gen-sep))
	    (dolist (class-file (cadr lang))
	      (insert sep (format class-file (car class-def)))
	      (setq sep " "))))
	(dolist (evocab (caaddr dep))
	  (let ((sep gen-sep))
	    (dolist (vocab-file (cons (car antlr-special-file-formats)
				      (car lang)))
	      (insert sep (format vocab-file evocab))
	      (setq sep " "))))
	(antlr-makefile-insert-variable n "\n$(" ")")
	(insert ": " (car dep))
	(dolist (ivocab (cdaddr dep))
	  (insert " " (format (car antlr-special-file-formats) ivocab)))
	(let ((glibs (antlr-superclasses-glibs supers classes)))
	  (if (cadr glibs) (setq with-error t))
	  (dolist (super (cddr glibs))
	    (insert " " (car super))
	    (if (cdr super)
		(insert " " (format (car antlr-special-file-formats)
				    (cdr super)))))
	  (insert "\n\t"
		  (caddr antlr-makefile-specification)
		  (car glibs)
		  " $<\n"
		  (car antlr-makefile-specification)))))
    (if n
	(let ((i 0))
	  (antlr-makefile-insert-variable nil "" " =")
	  (while (<= (incf i) n)
	    (antlr-makefile-insert-variable i " $(" ")"))
	  (insert "\n" (car antlr-makefile-specification))))
    (if (string-equal (car antlr-makefile-specification) "\n")
	(backward-delete-char 1))
    (when with-error
      (goto-char (point-min))
      (insert antlr-help-unknown-file-text))
    (unless in-makefile
      (copy-region-as-kill (point-min) (point-max))
      (goto-char (point-min))
      (insert (format antlr-help-rules-intro dirname)))))

;;;###autoload
(defun antlr-show-makefile-rules ()
  "Show Makefile rules for all grammar files in the current directory.
If the `major-mode' of the current buffer has the value `makefile-mode',
the rules are directory inserted at point.  Otherwise, a *Help* buffer
is shown with the rules which are also put into the `kill-ring' for
\\[yank].

This command considers import/export vocabularies and grammar
inheritance and provides a value for the \"-glib\" option if necessary.
Customize variable `antlr-makefile-specification' for the appearance of
the rules.

If the file for a super-grammar cannot be determined, special file names
are used according to variable `antlr-unknown-file-formats' and a
commentary with value `antlr-help-unknown-file-text' is added.  The
*Help* buffer always starts with the text in `antlr-help-rules-intro'."
  (interactive)
  (if (null (eq major-mode 'makefile-mode))
      (antlr-with-displaying-help-buffer 'antlr-insert-makefile-rules)
    (push-mark)
    (antlr-insert-makefile-rules t)))


;;;===========================================================================
;;;  Indentation
;;;===========================================================================

(defun antlr-indent-line ()
  "Indent the current line as ANTLR grammar code.
The indentation of non-comment lines are calculated by `c-basic-offset',
multiplied by:
 - the level of the paren/brace/bracket depth,
 - plus 0/2/1, depending on the position inside the rule: header, body,
   exception part,
 - minus 1 if `antlr-indent-item-regexp' matches the beginning of the
   line starting from the first non-blank.

Lines inside block comments are not changed or indented by
`c-indent-line', see `antlr-indent-comment'."
  (let ((orig (point)) bol boi indent syntax)
    (beginning-of-line)
    (setq bol (point))
    (skip-chars-forward " \t")
    (setq boi (point))
    ;; check syntax at beginning of indentation ------------------------------
    (antlr-with-syntax-table antlr-action-syntax-table
      (antlr-invalidate-context-cache)
      (cond ((symbolp (setq syntax (antlr-syntactic-context)))
	     (setq indent nil))		; block-comments, strings, (comments)
	    ((eq (char-after) ?#)	; cpp directive
	     (setq syntax 'cpp)
	     (setq indent 0))		; indentation at 0
	    ((progn
	       (antlr-next-rule -1 t)
	       (if (antlr-search-forward ":") (< boi (1- (point))) t))
	     (setq indent 0))		; in rule header
	    ((if (antlr-search-forward ";") (< boi (point)) t)
	     (setq indent 2))		; in rule body
	    (t
	     (forward-char)
	     (antlr-skip-exception-part nil)
	     (setq indent (if (> (point) boi) 1 0))))) ; in exception part?
    ;; compute the corresponding indentation and indent ----------------------
    (if (null indent)
	(progn
	  (goto-char orig)
	  (and (eq antlr-indent-comment t)
	       (not (eq syntax 'string))
	       (c-indent-line)))
      ;; do it ourselves
      (goto-char boi)
      (unless (symbolp syntax)		; direct indentation
	(antlr-invalidate-context-cache)
	(incf indent (antlr-syntactic-context))
	(and (> indent 0) (looking-at antlr-indent-item-regexp) (decf indent))
	(setq indent (* indent c-basic-offset)))
      ;; the usual major-mode indent stuff -----------------------------------
      (setq orig (- (point-max) orig))
      (unless (= (current-column) indent)
	(delete-region bol boi)
	(beginning-of-line)
	(indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) orig) (point))
	  (goto-char (- (point-max) orig))))))

(defun antlr-indent-command (&optional arg)
  "Indent the current line or insert tabs/spaces.
With optional prefix argument ARG or if the previous command was this
command, insert ARG tabs or spaces according to `indent-tabs-mode'.
Otherwise, indent the current line with `antlr-indent-line'."
  (interactive "P")
  (if (or arg (eq last-command 'antlr-indent-command))
      (insert-tab arg)
    (let ((antlr-indent-comment (and antlr-indent-comment t))) ; dynamic
      (antlr-indent-line))))


;;;===========================================================================
;;;  Mode entry
;;;===========================================================================

(defun antlr-c-common-init ()
  "Like `c-common-init' except menu, auto-hungry and c-style stuff."
  ;; X/Emacs 20 only
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'imenu-generic-expression) ;set in the mode functions
  (and (boundp 'comment-line-break-function)
       (make-local-variable 'comment-line-break-function))
  ;; Emacs 19.30 and beyond only, AFAIK
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'c-fill-paragraph)))
  ;; now set their values
  (setq paragraph-start (concat page-delimiter "\\|$")
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline t
	parse-sexp-ignore-comments t
	indent-line-function 'c-indent-line
	indent-region-function 'c-indent-region
	outline-regexp "[^#\n\^M]"
	outline-level 'c-outline-level
	comment-column 32
	comment-start-skip "/\\*+ *\\|// *"
	comment-multi-line nil
	comment-line-break-function 'c-comment-line-break-function)
  ;; we have to do something special for c-offsets-alist so that the
  ;; buffer local value has its own alist structure.
  (setq c-offsets-alist (copy-alist c-offsets-alist))
  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent))

(defun antlr-language-for-option (option-value)
  "Find element in `antlr-language-alist' for OPTION-VALUE."
  ;; Like (find OPTION-VALUE antlr-language-alist :key 'cddr :test 'member)
  (let ((seq antlr-language-alist)
	r)
    (while seq
      (setq r (pop seq))
      (if (member option-value (cddr r))
	  (setq seq nil)		; stop
	(setq r nil)))			; no result yet
    r))

;;;###autoload
(defun antlr-mode ()
  "Major mode for editing ANTLR grammar files.
\\{antlr-mode-map}"
  (interactive)
  (c-initialize-cc-mode)		; for java syntax table
  (kill-all-local-variables)
  ;; ANTLR specific ----------------------------------------------------------
  (setq major-mode 'antlr-mode
	mode-name "Antlr")
  (setq local-abbrev-table antlr-mode-abbrev-table)
  (unless antlr-mode-syntax-table
    (setq antlr-mode-syntax-table (make-syntax-table))
    (c-populate-syntax-table antlr-mode-syntax-table))
  (set-syntax-table antlr-mode-syntax-table)
  (unless antlr-action-syntax-table
    (let ((slist (nth 3 antlr-font-lock-defaults)))
      (setq antlr-action-syntax-table
	    (copy-syntax-table antlr-mode-syntax-table))
      (while slist
	(modify-syntax-entry (caar slist) (cdar slist)
			     antlr-action-syntax-table)
	(setq slist (cdr slist)))))
  (use-local-map antlr-mode-map)
  (make-local-variable 'antlr-language)
  (unless antlr-language
    (save-excursion
      (goto-char (point-min))
      (setq antlr-language
	    (car (or (and (re-search-forward (cdr antlr-language-limit-n-regexp)
					(car antlr-language-limit-n-regexp)
					t)
			  (antlr-language-for-option (match-string 1)))
		     (antlr-language-for-option nil))))))
  (if (stringp (cadr (assq antlr-language antlr-language-alist)))
      (setq mode-name
	    (concat "Antlr."
		    (cadr (assq antlr-language antlr-language-alist)))))
  ;; indentation, for the C engine -------------------------------------------
  (antlr-c-common-init)
  (setq indent-line-function 'antlr-indent-line
	indent-region-function nil)	; too lazy
  (setq comment-start "// "
 	comment-end "")
  (c-set-style "java")
  (if (eq antlr-language 'c++-mode)
      (setq c-conditional-key c-C++-conditional-key
	    c-comment-start-regexp c-C++-comment-start-regexp
	    c-class-key c-C++-class-key
	    c-extra-toplevel-key c-C++-extra-toplevel-key
	    c-access-key c-C++-access-key
	    c-recognize-knr-p nil)
    (setq c-conditional-key c-Java-conditional-key
	  c-comment-start-regexp c-Java-comment-start-regexp
	  c-class-key c-Java-class-key
	  c-method-key nil
	  c-baseclass-key nil
	  c-recognize-knr-p nil
	  c-access-key (and (boundp 'c-Java-access-key) c-Java-access-key))
    (and (boundp 'c-inexpr-class-key) (boundp 'c-Java-inexpr-class-key)
	 (setq c-inexpr-class-key c-Java-inexpr-class-key)))
  ;; various -----------------------------------------------------------------
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults antlr-font-lock-defaults)
  (easy-menu-add antlr-mode-menu)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'antlr-imenu-create-index-function)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression t)	; fool stupid test
  (and antlr-imenu-name			; there should be a global variable...
       (fboundp 'imenu-add-to-menubar)
       (imenu-add-to-menubar
	(if (stringp antlr-imenu-name) antlr-imenu-name "Index")))
  (antlr-set-tabs)
  (run-hooks 'antlr-mode-hook))

;; A smarter version of `group-buffers-menu-by-mode-then-alphabetically' (in
;; XEmacs) could use the following property.  The header of the submenu would
;; be "Antlr" instead of "Antlr.C++" or (not and!) "Antlr.Java".
(put 'antlr-mode 'mode-name "Antlr")

;;;###autoload
(defun antlr-set-tabs ()
  "Use ANTLR's convention for TABs according to `antlr-tab-offset-alist'.
Used in `antlr-mode'.  Also a useful function in `java-mode-hook'."
  (if buffer-file-name
      (let ((alist antlr-tab-offset-alist) elem)
	(while alist
	  (setq elem (pop alist))
	  (and (or (null (car elem)) (eq (car elem) major-mode))
	       (or (null (cadr elem))
		   (string-match (cadr elem) buffer-file-name))
	       (setq tab-width (caddr elem)
		     indent-tabs-mode (cadddr elem)
		     alist nil))))))

; LocalWords:  antlr ANother ANTLR's Cpp Lexer TreeParser esp refs VALUEs ea ee
; LocalWords:  Java's Nomencl ruledef tokendef ruleref tokenref setType ader ev
; LocalWords:  ivate syntab lexer treeparser lic rotected rivate bor boi AFAIK
; LocalWords:  slist knr inexpr unhide jit GENS SEP GEN sTokenTypes hpp cpp DEP
; LocalWords:  VOCAB EVOCAB Antlr's TokenTypes exportVocab incl excl SUPERS gen
; LocalWords:  VOCABS IVOCAB exportVocabs importVocabs superclasses vocab kens
; LocalWords:  sclass evocab ivocab importVocab deps glibs supers sep dep lang

;;; antlr-mode.el ends here
