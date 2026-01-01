;;; antlr-mode.el --- major mode for ANTLR grammar files  -*- lexical-binding: t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: Christoph Wedler <Christoph.Wedler@sap.com>
;; Keywords: languages, ANTLR, code generator
;; Version: 3.2.0
;; URL: https://antlr-mode.sourceforge.net/

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

;; The Emacs package ANTLR-Mode provides: syntax highlighting for ANTLR grammar
;; files, automatic indentation, menus containing rule/token definitions and
;; supported options and various other things like running ANTLR from within
;; Emacs.  It works for ANTLR v2, v3 and v4.

;; For details, check <https://antlr-mode.sourceforge.net/> or, if you prefer
;; the manual style, follow all commands mentioned in the documentation of
;; `antlr-mode'.  ANTLR is a LL(k)-based recognition tool which generates
;; lexers, parsers and tree transformers in Java, C++ or other languages and
;; can be found at <https://www.antlr.org/>.

;; Topics for 3.2 or later:
;;
;;  * Special support for `indent-region': faster and better for Python ELP
;;    profiling in a class init action shows half the time is spent in
;;    `antlr-next-rule', the other half in `c-guess-basic-syntax'.
;;    Do not define a indent command, just a function to be put into
;;    indent-line/region-function.
;;  * In v4, highlight lexer commands after "->"
;;  * Test: in `antlr-imenu-create-index-function', can we use
;;      (or antlr-skip-line-regexp antlr-grammar-header-regexp)
;;  * Use native menu bindings instead easymenu (and use :help)
;;  * Support v4 rule element options
;;  * Define minor mode for `antlr-hide-actions' functionality.
;;  * [C-c C-u].  *Help* for current rule / all rules: used-By list (at least
;;    for single-file grammars)
;;  * [C-c C-j].  Jump to generated coding.

;; Eventually:
;;
;;  * Support for one of the multi-mode imenu extensions mentioned in
;;    https://www.emacswiki.org/emacs-test/ImenuMode - if necessary
;;  * [C-c C-w].  Produce HTML document with syntax highlighted and
;;    hyper-links.  With htmfontify: invisible actions did not really work
;;    (only w/o spaces?, default invisible would be better anyway) - we need to
;;    set <a> tags afterwards ourselves...  Firefox does not understand
;;    encoding via XML declaration - use HTML meta tag.
;;  * Support for outline-minor-mode.

;; The following topics and suggestions are unlikely to be implemented:
;;
;;  * Some constructs of languages (in actions) which are highly un-C-ish might
;;    bring Emacs (and ANTLR!) out of sync: e.g. regexp literals in Perl,
;;    character and percent literals in Ruby.
;;  * Faster syntax highlighting: sectionize the buffer into Antlr and action
;;    code and run special highlighting functions on these regions.  UNLIKELY
;;    due to: code size, this mode would depend on font-lock internals.
;;  * Set the syntax-table of the inner mode before calling the indentation
;;    engine of the inner mode (possible? - BUT: actions should still end at
;;    the same place!).  Probably not worth the effort.

;; Bug fixes, bug reports, improvements, and suggestions for the newest version
;; are strongly appreciated.

;;; Installation:

;; This file requires Emacs-24.3 or higher which already includes a version of
;; antlr-mode.  If you want to use this (hopefully newer) version instead, put
;; this file into a directory early in `load-path' (or `push' a new one to it)
;; and M-x byte-compile-file this file file.

;; If you want to use this mode with ANTLR v4 files, put the following into
;; your init file:
;;   (autoload 'antlr-v4-mode "antlr-mode" nil t)
;;   (push '("\\.g4\\'" . antlr-v4-mode) auto-mode-alist)

;; To customize, use menu item "Antlr" -> "Customize Antlr".

;;; Development resources:

;; Good examples for different action languages (for syntax coloring, ...), but
;; there are almost no examples with rule parameters and return values...

;;  * examples-v3-master/Delphi: C/C.g, IslandGrammar/Simple.g, Python/Python.g
;;  * examples-v3-master/JavaScript: island-grammar/Simple.g, python/Python.g
;;  * examples-v3-master/Python: C/C.g, island-grammar/Simple.g, python/Python.g
;;  * Ruby in antlr3-master/samples/standard: ../CPP.g, ../JavaScript.g, C/C.g,
;;    python/Python.g
;;  * grammars-v4-master/

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'compile))
(when (< emacs-major-version 28)  ; preloaded in Emacs 28
  (require 'easymenu))
(require 'cc-mode)
(require 'cl-seq) ; for cl-find, whose autoload form is in cl-loaddefs.el

(defvar outline-level)
(defvar imenu-use-markers)
(defvar imenu-create-index-function)

(defalias 'antlr-c-forward-sws #'c-forward-sws)


;;;;##########################################################################
;;;;  Variables
;;;;##########################################################################


(defgroup antlr nil
  "Major mode for ANTLR grammar files."
  :group 'languages
  :link '(emacs-commentary-link "antlr-mode.el")
  :link '(url-link "https://antlr-mode.sourceforge.net/")
  :prefix "antlr-")

(defconst antlr-version "3.2.0"
  "ANTLR major mode version number.
Check <https://antlr-mode.sourceforge.net/> for the newest.")

(defcustom antlr-language-limit-n-regexp ; TODO: rename? (also for tool-version)
  ;; actually, it is in v2 "L" only, in v3/v4 'L' only
  '(30000 . "\\<language[ \t]*=[ \t]*[\"']?\\([A-Z][A-Za-z_0-9]*\\)[\"']?")
  "Used to set a reasonable value for `antlr-language'.
Looks like \(LIMIT .  REGEXP).  Search for REGEXP from the beginning of
the buffer to LIMIT and use the first group in the matched string to set
the language according to `antlr-language-list'."
  :type '(cons (choice :tag "Limit" (const :tag "No" nil) (integer :value 0))
	       regexp))


;;;===========================================================================
;;;  Controlling the tool to use, automatically deduced
;;;===========================================================================

(defcustom antlr-tool-version nil
  "The version symbol of the Antlr tool.  DO NOT CUSTOMIZE.
Set as buffer-local in buffers using `antlr-mode'.  Supported local
values are `antlr-v2', `antlr-v3' and`antlr-v4'.  The value is used to
set other variables, see `antlr-tool-version-variables'."
  :type '(radio (const :tag "Automatic" nil)
		(sexp :tag "Do not choose this" :value nil)))

(defvar antlr-tool-version-variables
  '(antlr-tool-mode-name
    antlr-tool-command
    antlr-language-list
    antlr-syntax-propertize
    antlr-options-alists
    &optional
    antlr-grammar-header-regexp
    antlr-compilation-error-regexp-alist
    antlr-skip-line-regexp
    antlr-rule-postlude-skip-alist
    antlr-rule-postlude-skip-regexp
    antlr-ruleref-assign-regexp
    antlr-font-lock-symbol-regexp)
  "List of variables which have a tool-dependent value.
For each antlr-VAR in this list, function `antlr-set-local-variables'
makes it buffer-local and uses the variable VERSION-VAR, i.e.,
antlr-v2-VAR,  antlr-v3-VAR or antlr-v4-VAR to set its value,
depending on the value VERSION of `antlr-tool-version'.

If that variable VERSION-VAR does not exist, ignore antlr-VAR if it is
listed after the symbol &optional, or issue an error otherwise.")

(defvar antlr-tool-mode-name nil
  "The first part of the mode name used in the mode line.
The value is tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-tool-mode-name "Antlr4"
  "Value for `antlr-tool-mode-name' when using ANTLR v4.")
(defvar antlr-v3-tool-mode-name "Antlr3"
  "Value for `antlr-tool-mode-name' when using ANTLR v3.")
(defvar antlr-v2-tool-mode-name "Antlr2"
  "Value for `antlr-tool-mode-name' when using ANTLR v2.")

(defvar antlr-language-list nil
  "Alist of supported action languages.
Each element in this list looks like (LANG-SYMBOL STRING...) where
LANG-SYMBOL is made the value of `antlr-language' and one of the STRINGs
is used in ANTLR's `language` grammar/file option to specify that
language.
The value is tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-language-list
  '((antlr-java "Java") (antlr-cpp "Cpp") ; +CSharp
    (antlr-js "JavaScript") (antlr-python "Python2" "Python3"))
  ;; in ANTLR4-codegen/runtime, but not here
  ;; - no standard Emacs major modes: CSharp, Go, Swift
  "Value for `antlr-language-list' when using ANTLR v4.")

(defvar antlr-v3-language-list
  '((antlr-java "Java") (antlr-cpp "Cpp") (antlr-c "C")
    (antlr-objc "ObjC")                 ; + CSharp
    (antlr-js "JavaScript") (antlr-delphi "Delphi") ; + Perl
    (antlr-python "Python" "Python3") (antlr-ruby "Ruby"))
  ;; in ANTLR3-codegen/runtime, but not here
  ;; - no standard Emacs major modes: ActionScript, CSharp2, CSharp3
  ;; - I didn't spend time for it (and do not plan to do so): Perl5
  "Value for `antlr-language-list' when using ANTLR v3.")

(defvar antlr-v2-language-list
  '((antlr-java "Java") (antlr-cpp "Cpp") (antlr-python "Python")
    (nil "HTML") (nil "Diagnostic"))    ; + CSharp
  "Value for `antlr-language-list' when using ANTLR v2.")


;;;===========================================================================
;;;  Controlling ANTLR's code generator (language option)
;;;===========================================================================

(defvar antlr-language nil
  "Major mode corresponding to ANTLR's \"language\" option.
Set via `antlr-language-list'.  The only useful place to change this
buffer-local variable yourself is in `antlr-mode-hook' or in the \"local
variable list\" near the end of the file, see
`enable-local-variables'.
The value is used to set other variables, see `antlr-language-variables'.")

(defvar antlr-language-variables
  '(antlr-language-mode-name
    antlr-action-mode
    &optional
    antlr-init-cc-mode
    antlr-init-submode
    antlr-indent-action-line
    antlr-action-font-lock-keywords
    antlr-action-names)
    "List of variables which have a language-dependent value.
For each antlr-VAR in this list, function `antlr-set-local-variables'
makes it buffer-local and uses the variable LANGUAGE-VAR, i.e.,
antlr-java-VAR,  antlr-cpp-VAR, and so on to set its value,
depending on the value LANGUAGE of `antlr-language'.

If that variable LANGUAGE-VAR does not exist, ignore antlr-VAR if it is
listed after the symbol &optional, or issue an error otherwise.")

;; Languages other than Java are defined at the end --------------------------

(defvar antlr-language-mode-name "txt"
  "The second part of the mode name used in the mode line.
The value is language-dependent, see `antlr-language-variables'.")

(defvar antlr-java-language-mode-name "Java"
  "Value for `antlr-language-mode-name' when using language `antlr-java'.")

(defvar antlr-action-mode nil
  "Major-mode for code in actions of the grammar.
The value is language-dependent, see `antlr-language-variables'.")

(defvar antlr-java-action-mode 'java-mode
  "Value for `antlr-action-mode' when using language `antlr-java'.")

(defvar antlr-init-cc-mode 'java-mode
  "Major-mode used to initialize the language variables of CC Mode.
Used as argument for `c-init-language-vars-for'.
The value is language-dependent, see `antlr-language-variables'.")

(defvar antlr-java-init-cc-mode 'java-mode
  "Value for `antlr-init-cc-mode' when using language `antlr-java'.")

(defvar antlr-init-submode #'antlr-set-tabs
  "Function used to initialize the action language.
Important for languages which do not depend on CC Mode.
The value is language-dependent, see `antlr-language-variables'.")

(defcustom antlr-language-alist         ; is obsolete now
  nil
  "List of ANTLR's supported languages.  Variable is UNUSED.
Each element in this list looks like
  (MAJOR-MODE MODELINE-STRING OPTION-VALUE...)

MAJOR-MODE, the major mode of the code in the grammar's actions, is the
value of `antlr-language' if the first group in the string matched by
REGEXP in `antlr-language-limit-n-regexp' is one of the OPTION-VALUEs.
An OPTION-VALUE of nil denotes the fallback element.  MODELINE-STRING is
also displayed in the mode line next to \"Antlr\"."
  :type '(sexp :tag "DO NOT CUSTOMIZE" :value nil))


;;;===========================================================================
;;;  Hide/Unhide, Indent/Tabs
;;;===========================================================================

(defcustom antlr-action-visibility 3
  "Visibility of actions when command `antlr-hide-actions' is used.
If nil, the actions with their surrounding braces are hidden.  If a
number, do not hide the braces, only hide the contents if its length is
greater than this number."
  :type '(choice (const :tag "Completely hidden" nil)
		 (integer :tag "Hidden if longer than" :value 3)))

(defcustom antlr-indent-comment 'tab
  "Non-nil, if the indentation should touch lines in block comments.
If nil, no continuation line of a block comment is changed.  If t, they
are changed according to `c-indent-line'.  When not nil and not t,
they are only changed by \\[antlr-indent-command]."
  :type '(radio (const :tag "No" nil)
		(const :tag "Always" t)
		(sexp :tag "With TAB" :format "%t" :value tab)))

(defcustom antlr-tab-offset-alist       ; TODO: still advertise?
  '((antlr-mode nil 4 nil)
    (java-mode "antlr" 4 nil))
  "Alist to determine whether to use ANTLR's convention for TABs.
Each element looks like \(MAJOR-MODE REGEXP TAB-WIDTH INDENT-TABS-MODE).
The first element whose MAJOR-MODE is nil or equal to `major-mode' and
whose REGEXP is nil or matches variable `buffer-file-name' is used to
set `tab-width' and `indent-tabs-mode'.  This is useful to support both
ANTLR's and Java's indentation styles.  Used by `antlr-set-tabs'."
  :type '(repeat (group :value (antlr-mode nil 8 nil)
			(choice (const :tag "All" nil)
				(function :tag "Major mode"))
			(choice (const :tag "All" nil) regexp)
			(integer :tag "Tab width")
			(boolean :tag "Indent-tabs-mode"))))

(defcustom antlr-indent-style "java"
  "If non-nil, cc-mode indentation style used for `antlr-mode'.
See `c-set-style' and for details, where the most interesting part in
`c-style-alist' is the value of `c-basic-offset'."
  :type '(choice (const nil) regexp))

(defvar antlr-base-offset-alist         ; TODO: make a defcustom?
  '((:header . 0) (:body . 2) (:exception . 1))
  "Influence the rule indentation of `antlr-indent-line'.
The default indentation of grammar lines are calculated by
`c-basic-offset', multiplied by:
 - the level of the paren/brace/bracket depth,
 - plus 0/2/1, depending on the position POS-SYMBOL inside the rule:
   :header, :body, :exception part, customized by this variable.
 - minus 1 if `antlr-indent-item-regexp' matches the beginning of the
   line starting from the first non-whitespace.

Each element in this list is an element (POS-SYMBOL . OFFSET).
The following POS-SYMBOL can be:
 - `:header', the rule header before `antlr-rule-body-start-op',
 - `:colon' for `antlr-rule-body-start-op', the character starting
   the rule body,
 - `:body`, the rule body starting at `antlr-rule-body-start-op'
   and ending with ';'
 - `:exception', the part of the rule after the ';', see function
   `antlr-skip-rule-postlude'.

`:header', `:body` and `:exception' must appear in the alist,
`:colon' is optional and its OFFSET defaults to the one from `:body`.")

(defcustom antlr-indent-item-regexp
  "[]}):;|]"
  "Regexp matching lines which should be indented by one TAB less.
See `antlr-indent-line' and command \\[antlr-indent-command]."
  :type 'regexp)

(defcustom antlr-indent-at-bol-alist    ; TODO: make this pure custom option (define language-dependent vars as defaults)
  ;; eval-when-compile not usable with defcustom...
  '((java-mode . "\\(package\\|import\\)\\_>")
    (c++-mode . "#\\(assert\\|cpu\\|define\\|endif\\|el\\(if\\|se\\)\\|i\\(dent\\|f\\(def\\|ndef\\)?\\|mport\\|nclude\\(_next\\)?\\)\\|line\\|machine\\|pragma\\|system\\|un\\(assert\\|def\\)\\|warning\\)\\_>")
    (c-mode . "#\\(assert\\|cpu\\|define\\|endif\\|el\\(if\\|se\\)\\|i\\(dent\\|f\\(def\\|ndef\\)?\\|mport\\|nclude\\(_next\\)?\\)\\|line\\|machine\\|pragma\\|system\\|un\\(assert\\|def\\)\\|warning\\)\\_>"))
  "Alist of regexps matching lines are indented at column 0.
Each element in this list looks like (MODE . REGEXP) where MODE is a
function and REGEXP is a regular expression.

If the value of `antlr-action-mode' equals to a MODE, the line starting
at the first non-whitespace is matched by the corresponding REGEXP, and
the line is part of a header action, indent the line at column 0 instead
of according to the normal rules of `antlr-indent-line'."
  :type '(repeat (cons (function :tag "Major mode") regexp)))

(defvar antlr-indent-action-line nil
  ;; TODO: better call it with action start?
  "Function which indents the current line in actions.
The function is called with the character address of the '{' starting
the action.
If nil, use CC mode to indent the line.
The value might be language-dependent, see `antlr-language-variables'.")

;; adopt indentation to cc-engine
(defvar antlr-disabling-cc-syntactic-symbols
  '(statement-block-intro
    defun-block-intro topmost-intro statement-case-intro member-init-intro
    arglist-intro brace-list-intro knr-argdecl-intro inher-intro
    objc-method-intro
    block-close defun-close class-close brace-list-close arglist-close
    inline-close extern-lang-close namespace-close)
  "CC Mode syntactic context symbols adopting the indentation by CC Mode.")


;;;===========================================================================
;;;  Options: customization
;;;===========================================================================

(defcustom antlr-end-of-defun-is-next nil
  "Non-nil, if rule movement commands normally jump to beginning of rule.
If non-nil, both `antlr-end-of-rule' and `antlr-end-of-rule' jump
to beginning of a rule with no or positive prefix arg, and to the
end of a rule with negative prefix arg.

Default nil means: `antlr-end-of-rule' jumps to beginning of a
rule, `antlr-end-of-rule' jumps to end of a rule."
  :type 'boolean)

(defcustom antlr-options-use-submenus t
  "Non-nil, if the major mode menu should include option submenus.
If nil, the menu just includes a command to insert options.  Otherwise,
it includes four submenus to insert file/grammar/rule/subrule options."
  :type 'boolean)

(defcustom antlr-options-auto-colon t
  "Non-nil, if `:' is inserted with a rule or subrule options section.
A `:' is only inserted if this value is non-nil, if a rule or subrule
option is inserted with \\[antlr-insert-option], if there was no rule or
subrule options section before, and if a `:' is not already present
after the section, ignoring whitespace, comments and the init action."
  :type 'boolean)

(defcustom antlr-options-style nil      ; TODO: obsolete
  "Obsolete user option."
  :type '(repeat (symbol :tag "Style symbol")))

(defcustom antlr-options-push-mark t
  "Non-nil, if inserting an option should set & push mark.
If nil, never set mark when inserting an option with command
\\[antlr-insert-option].  If t, always set mark via `push-mark'.  If a
number, only set mark if point was outside the options area before and
the number of lines between point and the insert position is greater
than this value.  Otherwise, only set mark if point was outside the
options area before."
  :type '(radio (const :tag "No" nil)
		(const :tag "Always" t)
		(integer :tag "Lines between" :value 10)
		(sexp :tag "If outside options" :format "%t" :value outside)))

(defcustom antlr-options-assign-string " = "
  "String containing `=' to use between option name and value.
This string is only used if the option to insert did not exist before
or if there was no `=' after it.  In other words, the spacing around an
existing `=' won't be changed when changing an option value."
  :type 'string)


;;;===========================================================================
;;;  Options: definitions
;;;===========================================================================

(defvar antlr-options-headings '("file" "grammar" "rule" "subrule")
  "Headings for the four different option kinds.
The standard value is (\"file\" \"grammar\" \"rule\" \"subrule\").  See
`antlr-options-alists'")

(defvar antlr-options-alists nil
  ;; TODO: distinguish between "no known option" (options{} is allowed), and
  ;; does not exist (file options in v3 and v4)
  "Definitions for Antlr's options of all four different kinds.

The value looks like \(FILE GRAMMAR RULE SUBRULE) where each FILE,
GRAMMAR, RULE, and SUBRULE is a list of option definitions of the
corresponding kind, i.e., looks like \(OPTION-DEF...).

Each OPTION-DEF looks like \(OPTION-NAME EXTRA-FN VALUE-SPEC...) which
defines a file/grammar/rule/subrule option with name OPTION-NAME.  The
OPTION-NAMEs are used for the creation of the \"Insert XXX Option\"
submenus, see `antlr-options-use-submenus', and to allow the insertion
of the option name with completion when using \\[antlr-insert-option].

If EXTRA-FN is a function, it is called at different phases of the
insertion with arguments \(PHASE OPTION-NAME).  PHASE can have the
values `before-input' or `after-insertion', additional phases might be
defined in future versions of this mode.  The phase `before-input'
occurs before the user is asked to insert a value.  The phase
`after-insertion' occurs after the option value has been inserted.
EXTRA-FN might be called with additional arguments in future versions of
this mode.

Each specification VALUE-SPEC looks like \(VERSION READ-FN ARG...).  The
last VALUE-SPEC in an OPTION-DEF whose VERSION is smaller or equal to
`antlr-tool-version' specifies how the user is asked for the value of
the option.

If READ-FN is nil, the only ARG is a string which is printed at the echo
area to guide the user what to insert at point.  Otherwise, READ-FN is
called with arguments \(INIT-VALUE ARG...) to get the new value of the
option.  INIT-VALUE is the old value of the option or nil.

The standard value contains the following functions as READ-FN:
`antlr-read-value' with ARGs = \(PROMPT AS-STRING TABLE) which reads a
general value, or `antlr-read-boolean' with ARGs = \(PROMPT TABLE) which
reads a boolean value or a member of TABLE.  PROMPT is the prompt when
asking for a new value.  If non-nil, TABLE is a table for completion or
a function evaluating to such a table.  The return value is quoted if
AS-STRING is non-nil.

The value is tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-options-alists
  ;; see https://github.com/antlr/antlr4 - doc/options.md
  '(()                                  ; no file options
    (;; grammar options ------------------------------------------------------
     ("language"
      ;; The target language for code generation. Default is Java. See Code
      ;; Generation Targets for list of currently supported target languages.
      antlr-language-option-extra antlr-read-language "Generated language: ")
     ("tokenVocab"
      ;; Where ANTLR should get predefined tokens and token types. Tree
      ;; grammars need it to get the token types from the parser that creates
      ;; its trees. Default value: Do not import token vocab.
      nil antlr-read-value "Token vocabulary: ")
     ("TokenLabelType"                  ; parser and tree only
      ;; Set the type of all tree labels and tree-valued expressions. Without
      ;; this option, trees are of type Object. TODO: Cross-reference default
      ;; impl (org.antlr.runtime.tree.CommonTree in Java)?
      nil antlr-read-value "Token type: ")
     ("superClass"                      ; in combined grammar: for parser
      ;; Set the superclass of the generated recognizer. Default value
      ;; Lexer/Parser/TreeParser (org.antlr.runtime.Parser in Java)?
      nil antlr-read-value "Super class: ")
     ("contextSuperClass"
      nil antlr-read-value "Rule context super class: ")
     ("exportMacro"                     ; doc/cpp-target.md
      antlr-c++-mode-extra antlr-read-value "Export macro: "))
    nil                                 ; no rule option yet
    nil                                 ; no subrule options
    ;; use ??, *?, +? for non-greedy subrules
    ;; (but there are some greedy options in grammars-v4-master/...)
     ;; rule element options in v4 are actually different (and have a different
     ;; syntax): <optionname = value> - not yet supported
     ;; after op: <assoc=left|right>
     ;; after sempred: <fail={expr}>
    )
  "Value for `antlr-options-alists' when using ANTLR v4.")

;;; v3:
;; https://theantlrguy.atlassian.net/wiki/display/ANTLR3/ANTLR+3+Wiki+Home

;; $ANTLR3/tool/src/main/java/org/antlr/tool/Grammar.java
;; https://theantlrguy.atlassian.net/wiki/display/ANTLR3/Grammar+options
;; https://theantlrguy.atlassian.net/wiki/display/ANTLR3/Rule+and+subrule+options
(defvar antlr-v3-options-alists
  '(()                                  ; no file options
    (;; grammar options ------------------------------------------------------
     ("language"
      ;; The target language for code generation. Default is Java. See Code
      ;; Generation Targets for list of currently supported target languages.
      antlr-language-option-extra antlr-read-language "Generated language: ")
     ("tokenVocab"
      ;; Where ANTLR should get predefined tokens and token types. Tree
      ;; grammars need it to get the token types from the parser that creates
      ;; its trees. Default value: Do not import token vocab.
      nil antlr-read-value "Token vocabulary: ")
     ("output"                          ; parser and tree only
      ;; The type of output the generated parser should return. Valid values
      ;; are AST and template. TODO: Briefly, what are the interpretations of
      ;; these values? Default value: nothing
      nil antlr-read-value "Output type (AST or template): ") ; TODO: completion
     ("TokenLabelType"                  ; parser and tree only
      ;; Set the type of all tree labels and tree-valued expressions. Without
      ;; this option, trees are of type Object. TODO: Cross-reference default
      ;; impl (org.antlr.runtime.tree.CommonTree in Java)?
      nil antlr-read-value "Token type: ")
     ("superClass"                      ; in combined grammar: for parser
      ;; Set the superclass of the generated recognizer. Default value
      ;; Lexer/Parser/TreeParser (org.antlr.runtime.Parser in Java)?
      nil antlr-read-value "Super class: ")
     ("filter"    ; lexer only
      ;; In the lexer, this allows you to try a list of lexer rules in
      ;; order. The first one that matches, wins. This is the token that
      ;; nextToken() returns. If nothing matches, the lexer consumes a single
      ;; character and tries the list of rules again. See Lexical filters for
      ;; more., Default: false
      nil antlr-read-boolean "Lexical filter? ")
     ("rewrite"                         ; parser and tree only
      ;; Valid values are true and false. Default is false. Use this option
      ;; when your translator output looks very much like the input. Your
      ;; actions can modify the TokenRewriteStream to insert, delete, or
      ;; replace ranges of tokens with another object. Used in conjunction with
      ;; output=template, you can very easily build translators that tweak
      ;; input files.
      nil antlr-read-value "Template rewrite: ")
     ("k"    ; parser and tree only
      ;; Limit the lookahead depth for the recognizer to at most k
      ;; symbols. This prevents the decision from using acyclic LL* DFA.
      nil antlr-read-value "Lookahead depth: ")
     ("backtrack"                       ; parser and tree only
      ;; Valid values are true and false. Default is false. Taken from
      ;; http://www.antlr.org:8080/pipermail/antlr-interest/2006-July/016818.html
      ;; : The new feature (a big one) is the backtrack=true option for
      ;; grammar, rule, and block that lets you type in any old crap and ANTLR
      ;; will backtrack if it can't figure out what you meant. No errors are
      ;; reported by antlr during analysis. It implicitly adds a syn pred in
      ;; front of every production, using them only if static grammar LL*
      ;; analysis fails. Syn pred code is not generated if the pred is not used
      ;; in a decision. This is essentially a rapid prototyping mode. It is
      ;; what I have used on the java.g. Oh, it doesn't memoize partial parses
      ;; (i.e. rule parsing results) during backtracking automatically now. You
      ;; must also say memoize=true. Can make a HUGE difference to turn on.
      nil antlr-read-boolean "Use automatic backtracking if necessary? ")
     ("memoize"                         ; parser and tree only
      ;; Valid values are true and false. When backtracking, remember whether
      ;; or not rule references succeed so that the same input position cannot
      ;; be parsed more than once by the same rule. This effectively guarantees
      ;; linear parsing when backtracking at the cost of more memory. TODO:
      ;; Default value: false
      nil antlr-read-boolean "Store backtracking calculations? "))
    (;; rule options ---------------------------------------------------------
     ("backtrack"
      nil antlr-read-boolean "Use automatic backtracking if necessary? ")
     ("memoize"
      nil antlr-read-boolean "Store backtracking calculations? "))
    (;; subrule options ------------------------------------------------------
     ("k"
      nil antlr-read-value "Lookahead depth: ")
     ("greedy"                          ; default true
      nil antlr-read-boolean "Make this optional/loop subrule greedy? ")))
  "Value for `antlr-options-alists' when using ANTLR v3.")

(defvar antlr-v2-options-alists
  '(;; file options ----------------------------------------------------------
    (("language"
      antlr-language-option-extra antlr-read-language "Generated language: ")
     ("mangleLiteralPrefix" nil
      antlr-read-value "Prefix for literals (default LITERAL_): " t)
     ("namespace" antlr-c++-mode-extra
      antlr-read-value "Wrap generated C++ code in namespace: " t)
     ("namespaceStd" antlr-c++-mode-extra
      antlr-read-value "Replace ANTLR_USE_NAMESPACE(std) by: " t)
     ("namespaceAntlr" antlr-c++-mode-extra
      antlr-read-value "Replace ANTLR_USE_NAMESPACE(antlr) by: " t)
     ("genHashLines" antlr-c++-mode-extra
      antlr-read-boolean "Include #line in generated C++ code? ")
     ("noConstructors" antlr-c++-mode-extra ; lexer only
      antlr-read-boolean "Omit default constructors for generated classes? ")
     )
    ;; grammar options --------------------------------------------------------
    (("k" nil
      antlr-read-value "Lookahead depth: ")
     ("importVocab" nil
      antlr-read-value "Import vocabulary: ")
     ("exportVocab" nil antlr-read-value
      "Export vocabulary: ")
     ("testLiterals" nil		; lexer only
      antlr-read-boolean "Test each token against literals table? ")
     ("defaultErrorHandler" nil		; not for lexer
      antlr-read-boolean "Generate default exception handler for each rule? ")
     ("codeGenMakeSwitchThreshold" nil
      antlr-read-value "Min number of alternatives for `switch': ")
     ("codeGenBitsetTestThreshold" nil
      antlr-read-value "Min size of lookahead set for bitset test: ")
     ("analyzerDebug" nil
      antlr-read-boolean "Display debugging info during grammar analysis? ")
     ("codeGenDebug" nil
      antlr-read-boolean "Display debugging info during code generation? ")
     ("buildAST" nil			; not for lexer
      antlr-read-boolean "Use automatic AST construction/transformation? ")
     ("ASTLabelType" nil		; not for lexer
      antlr-read-value "Class of user-defined AST node: " t)
     ("charVocabulary" nil		; lexer only
      nil "Insert character vocabulary")
     ("interactive" nil
      antlr-read-boolean "Generate interactive lexer/parser? ")
     ("caseSensitive" nil		; lexer only
      antlr-read-boolean "Case significant when matching characters? ")
     ("caseSensitiveLiterals" nil	; lexer only
      antlr-read-boolean "Case significant when testing literals table? ")
     ("classHeaderPrefix" nil
      nil "Initial String for grammar class definition")
     ("classHeaderSuffix" nil
      nil "Additional string for grammar class definition")
     ("filter" nil			; lexer only
      antlr-read-boolean "Skip rule (the name, true or false): "
      antlr-grammar-tokens)
     ("namespace" antlr-c++-mode-extra
      antlr-read-value "Wrap generated C++ code for grammar in namespace: " t)
     ("namespaceStd" antlr-c++-mode-extra
      antlr-read-value "Replace ANTLR_USE_NAMESPACE(std) by: " t)
     ("namespaceAntlr" antlr-c++-mode-extra
      antlr-read-value "Replace ANTLR_USE_NAMESPACE(antlr) by: " t)
     ("genHashLines" antlr-c++-mode-extra
      antlr-read-boolean "Include #line in generated C++ code? ")
     ("noConstructors" antlr-c++-mode-extra ; lexer only
      antlr-read-boolean "Omit default constructors for generated classes? ")
     )
    ;; rule options ----------------------------------------------------------
    (("testLiterals" nil		; lexer only
      antlr-read-boolean "Test this token against literals table? ")
     ("defaultErrorHandler" nil		; not for lexer
      antlr-read-boolean "Generate default exception handler for this rule? ")
     ("ignore" nil			; lexer only
      antlr-read-value "In this rule, ignore tokens of type: " nil
      antlr-grammar-tokens)
     ("paraphrase" nil			; lexer only
      antlr-read-value "In messages, replace name of this token by: " t)
     )
    ;; subrule options -------------------------------------------------------
    (("warnWhenFollowAmbig" nil
      antlr-read-boolean "Display warnings for ambiguities with FOLLOW? ")
     ("generateAmbigWarnings" nil
      antlr-read-boolean "Display warnings for ambiguities? ")
     ("greedy" nil
      antlr-read-boolean "Make this optional/loop subrule greedy? ")
     ))
  "Value for `antlr-options-alist' when using ANTLR v2.")


;;;===========================================================================
;;;  Run tool
;;;===========================================================================

(defvar antlr-tool-path nil             ; TODO: make it a defcustom?
  "Extra settings for environment variables $PATH and $LD_LIBRARY_PATH.")

(defvar antlr-compilation-mode nil
  "Mode used for compile output of \\[antlr-run-tool].")

(defvar antlr-compilation-error-regexp-alist nil
  "If non-nil, used instead `compilation-error-regexp-alist'`.
The value might be tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-compilation-error-regexp-alist
  '(("\\(?:[eE]rror\\|\\([wW]arning\\)\\)[ \t]*([0-9]+):[ \t]*\
\\([^\n:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4 (1))
    gnu)
  "Value for `antlr-compilation-error-regexp-alist' when using ANTLR v4.")

(defcustom antlr-run-tool-on-buffer-file t ; was nil before 3.2.0
  "Non-nil, if \\[antlr-run-tool] runs on the file for the current buffer.
If nil, the provided tool command must include the file name."
  :type 'boolean)

(defcustom antlr-tool-command nil
  "Command used in \\[antlr-run-tool] to run the Antlr tool.
This variable should include all options passed to Antlr except the
option \"-glib\" which is automatically suggested if necessary.

OBSOLETE as user option - customize version dependent user options."
  :type '(choice (const nil) string))

(defcustom antlr-v4-tool-command "java org.antlr.v4.Tool"
  ;; you probably also need to add s/th like
  ;; "-cp /usr/local/lib/antlr-4.6-complete.jar" to the string
  "Command used in \\[antlr-run-tool] to run the Antlr tool.
This variable should include all options passed to Antlr.
Value for `antlr-tool-command' when using ANTLR v4."
  :type 'string)

(defcustom antlr-v3-tool-command "java org.antlr.Tool"
  "Command used in \\[antlr-run-tool] to run the Antlr tool.
This variable should include all options passed to Antlr.
Value for `antlr-tool-command' when using ANTLR v3."
  :type 'string)

(defcustom antlr-v2-tool-command "java antlr.Tool"
  "Command used in \\[antlr-run-tool] to run the Antlr tool.
This variable should include all options passed to Antlr except the
option \"-glib\" which is automatically suggested if necessary.
Value for `antlr-tool-command' when using ANTLR v2."
  :type 'string)

(defcustom antlr-ask-about-save t
  "If not nil, \\[antlr-run-tool] asks which buffers to save.
Otherwise, it saves all modified buffers before running without asking."
  :type 'boolean)


;;;===========================================================================
;;;  Makefile creation (ANTLR v2 only)
;;;===========================================================================

;; TODO: make it a variable only (no `defcustom')
(defcustom antlr-makefile-specification
  '("\n" ("GENS" "GENS%d" " \\\n\t") "$(ANTLR)")
  "Variable to specify the appearance of the generated makefile rules.
This variable is only used or ANTLR v2 grammars.  For v3 and v4
grammars, run the ANTLR tool with option \"--depend\".

This variable influences the output of \\[antlr-show-makefile-rules].
It looks like \(RULE-SEP GEN-VAR-SPEC COMMAND).

RULE-SEP is the string to separate different makefile rules.  COMMAND is
a string with the command which runs the Antlr tool, it should include
all options except the option \"-glib\" which is automatically added
if necessary.

If GEN-VAR-SPEC is nil, each target directly consists of a list of
files.  If GEN-VAR-SPEC looks like \(GEN-VAR GEN-VAR-FORMAT GEN-SEP), a
Makefile variable is created for each rule target.

Then, GEN-VAR is a string with the name of the variable which contains
the file names of all makefile rules.  GEN-VAR-FORMAT is a format string
producing the variable of each target with substitution COUNT/%d where
COUNT starts with 1.  GEN-SEP is used to separate long variable values."
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
This variable is only used or ANTLR v2 grammars.

Each element in this list looks like
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
This variable is only used or ANTLR v2 grammars.

The value looks like \(VOCAB-FILE-FORMAT EXPANDED-GRAMMAR-FORMAT).

VOCAB-FILE-FORMAT is a format string, it specifies with substitution
VOCAB/%s the generated or input file for each export or import
vocabulary VOCAB, respectively.  EXPANDED-GRAMMAR-FORMAT is a format
string, it specifies with substitution GRAMMAR/%s the constructed
grammar file if the file GRAMMAR.g contains a grammar class which
extends a class other than \"Lexer\", \"Parser\" or \"TreeParser\".

See variable `antlr-file-formats-alist' for language dependent
formats.")

(defvar antlr-unknown-file-formats '("?%s?.g" "?%s?")
  "Formats which specify the names of unknown files.
This variable is only used or ANTLR v2 grammars.

The value looks like \(SUPER-GRAMMAR-FILE-FORMAT SUPER-EVOCAB-FORMAT).

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
This variable is only used or ANTLR v2 grammars.

See \\[antlr-show-makefile-rules] and `antlr-unknown-file-formats'.")

(defvar antlr-help-rules-intro
  "The following Makefile rules define the dependencies for all (non-
expanded) grammars in directory \"%s\".\n
They are stored in the kill-ring, i.e., you can insert them with C-y
into your Makefile.  You can also invoke \\[antlr-show-makefile-rules]
from within a Makefile to insert them directly.\n\n\n"
  "Introduction to use with \\[antlr-show-makefile-rules].
This variable is only used or ANTLR v2 grammars.

It is a format string and used with substitution DIRECTORY/%s where
DIRECTORY is the name of the current directory.")


;;;===========================================================================
;;;  Menu
;;;===========================================================================

(defcustom antlr-imenu-name t
  "Non-nil, if a \"Index\" menu should be added to the menubar.
If it is a string, it is used instead \"Index\".  Requires package
imenu.  For sorted menu entries, customize variable
`imenu-sort-function'."
  :type '(choice (const :tag "No menu" nil)
		 (const :tag "Index menu" t)
		 (string :tag "Other menu name")))

(defvar antlr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" #'antlr-indent-command)
    (define-key map "\e\C-a" #'antlr-beginning-of-rule)
    (define-key map "\e\C-e" #'antlr-end-of-rule)
    (define-key map "\C-c\C-a" #'antlr-beginning-of-body)
    (define-key map "\C-c\C-e" #'antlr-end-of-body)
    (define-key map "\C-c\C-f" 'subword-forward)
    (define-key map "\C-c\C-b" 'subword-backward)
    (define-key map "\C-c\C-c" #'comment-region)
    (define-key map "\C-c\C-k" #'antlr-insert-keyword-rule)
    (define-key map "\C-c\C-v" #'antlr-hide-actions)
    (define-key map "\C-c\C-r" #'antlr-run-tool)
    (define-key map "\C-c\C-o" #'antlr-insert-option)
    ;; I'm too lazy to define my own:
    (define-key map "\ea" #'c-beginning-of-statement)
    (define-key map "\ee" #'c-end-of-statement)
    ;; electric keys:
    (define-key map ":" #'antlr-electric-character)
    (define-key map ";" #'antlr-electric-character)
    (define-key map "|" #'antlr-electric-character)
    (define-key map "&" #'antlr-electric-character)
    (define-key map "(" #'antlr-electric-character)
    (define-key map ")" #'antlr-electric-character)
    (define-key map "{" #'antlr-electric-character)
    (define-key map "}" #'antlr-electric-character)
    map)
  "Keymap used in `antlr-mode' buffers.")

(easy-menu-define antlr-mode-menu antlr-mode-map
  "Major mode menu."
  `("Antlr"
    ,@(if antlr-options-use-submenus
	  `(("Insert File Option"
             :visible (elt antlr-options-alists 0)
	     :filter ,(lambda (x) (antlr-options-menu-filter 1 x)))
	    ("Insert Grammar Option"
             :visible (elt antlr-options-alists 1)
	     :filter ,(lambda (x) (antlr-options-menu-filter 2 x)))
	    ("Insert Rule Option"
             :visible (elt antlr-options-alists 2)
	     :filter ,(lambda (x) (antlr-options-menu-filter 3 x)))
	    ("Insert Subrule Option"
             :visible (elt antlr-options-alists 3)
	     :filter ,(lambda (x) (antlr-options-menu-filter 4 x)))
	    "---")
	'(["Insert Option" antlr-insert-option
	   :active (not buffer-read-only)]))
    ("Forward/Backward"
     ["Backward Rule" antlr-beginning-of-rule t]
     ["Forward Rule" antlr-end-of-rule t]
     ["Start of Rule Body" antlr-beginning-of-body
      :active (antlr-inside-rule-p)]
     ["End of Rule Body" antlr-end-of-body
      :active (antlr-inside-rule-p)]
     "---"
     ["Backward Statement" c-beginning-of-statement t]
     ["Forward Statement" c-end-of-statement t]
     ["Backward Subword" subword-forward t]
     ["Forward Subword" subword-backward t])
    ["Indent Region" indent-region
     :active (and (not buffer-read-only) (c-region-is-active-p))]
    ["Comment Out Region" comment-region
     :active (and (not buffer-read-only) (c-region-is-active-p))]
    ["Uncomment Region"
     (comment-region (region-beginning) (region-end) '(4))
     :active (and (not buffer-read-only) (c-region-is-active-p))]
    "---"
    ["Hide Actions (incl. Args)" antlr-hide-actions t]
    ["Hide Actions (excl. Args)" (antlr-hide-actions 2) t]
    ["Unhide All Actions" (antlr-hide-actions 0) t]
    "---"
    ["Run Tool on Grammar" antlr-run-tool t]
    ["Show Makefile Rules" antlr-show-makefile-rules (eq antlr-tool-version 'antlr-v2)]
    "---"
    ["Customize Antlr" (customize-group 'antlr) t]))


;;;===========================================================================
;;;  basic syntax
;;;===========================================================================

(defvar antlr-syntax-propertize nil
  "Specification used to apply \\+`syntax-table' text properties.
When non-nil, the value looks like (MAIN EXTEND-REGION MULTILINE-CHAR).

MAIN is used as value for `syntax-propertize-function'.

EXTEND-REGION is for `syntax-propertize-extend-region-functions';
it is appended to the existing value if it is a function, or
replaces the value otherwise, t leaves the value untouched.

MULTILINE-CHAR is for `c-multiline-string-start-char' if non-nil;
if that variable already has a non-nil value, it is set to t.

The value is tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-syntax-propertize
  '(antlr-syntax-propertize-charsets (antlr-syntax-propertize-wholerule))
  "Value for `antlr-syntax-propertize' when using ANTLR v4.")

(defvar antlr-v3-syntax-propertize
  '(antlr-syntax-propertize-template-literals syntax-propertize-multiline ?<)
  "Value for `antlr-syntax-propertize' when using ANTLR v3.")

(defvar antlr-v2-syntax-propertize nil
  "Value for `antlr-syntax-propertize' when using ANTLR v2.")

(defvar antlr-skip-line-regexp nil
  "Regexp matching special declarations after the grammar header.
The value is tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-skip-line-regexp "[ \t]*import[ \t]+[^][}{)(^\n;]+;"
  "Value for `antlr-skip-line-regexp' when using ANTLR v4.")

(defvar antlr-v3-skip-line-regexp "[ \t]*scope[ \t]+[^][}{)(^\n;]+;"
  "Value for `antlr-skip-line-regexp' when using ANTLR v3.")

(defvar antlr-rule-body-start-op ":"
  "Single-character string which starts the rule body.")


;;;===========================================================================
;;;  tool- and language-dependent font-lock
;;;===========================================================================

(defvar antlr-font-lock-symbol-regexp nil
  "Regexp matching symbol declarations in the grammar, or nil.
If a regexp, the buffer content matched by the first regexp group
is highlighted with face `antlr-keyword' and the content matched
by the second regexp group is highlighted with face
`antlr-symbol'.

The value is tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v4-font-lock-symbol-regexp
  "^[ \t]*\\(mode\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)?"
  "Value for `antlr-font-lock-symbol-regexp' when using ANTLR v4.")

(defvar antlr-v3-font-lock-symbol-regexp
  "^[ \t]*\\(scope\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\sw*\\)?"
  "Value for `antlr-font-lock-symbol-regexp' when using ANTLR v3.")

(defcustom antlr-font-lock-literal-regexp
    ;; actually, in v3/v4 it is 'L' only
  "\\([\"']\\)\\(\\sw\\(\\sw\\|-\\)*\\|\\(\\s_\\|\\s.\\)+\\|\\s(\\|\\s)\\)\\1"
  "Regexp matching literals with special syntax highlighting, or nil.
If nil, there is no special syntax highlighting for some literals.
Otherwise, it should be a regular expression which must contain at least
two regexp groups.  The string matched by the second group is highlighted
with face `antlr-literal'."
  :type '(choice (const :tag "None" nil) regexp))

(defvar antlr-font-lock-attribute-regexp "\\(\\$\\sw+\\)"
  "Regexp matching attributes within actions with special syntax highlighting.
If nil, there is no special syntax highlighting for attributes.
Otherwise, it should be a regular expression which must contain at least
one regexp group.  The string matched by the first group is highlighted
with face `antlr-attribute'.")

(defvar antlr-font-lock-negation-regexp "\\.\\.\\|\\([.~]\\)"
  "Regexp whose first regexp group matches negation.
The negation is highlighted with face `font-lock-negation-char-face'.")

(defvar antlr-font-lock-syntax-spec '("\\(->\\|[!^]\\)")
  "Specification for highlighting syntax symbols for AST creation: !, ^, ->.
If non-nil, the value looks like (REGEXP).  Syntax symbols are matched
by the first regexp group in REGEXP, and are highlighted with face
`antlr-syntax'.")
;; TODO: in v4, highlight lexer commands after "->"

(defvar antlr-grammar-header-regexp
  "\\<\\(lexer[ \t]+grammar\\|parser[ \t]+grammar\\|tree[ \t]+grammar\\|grammar\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\(?:\\sw\\|\\s_\\)*\\)[ \t]*;"
  "Regexp matching class headers.
The value might be tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v2-grammar-header-regexp
  "\\<\\(class\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\(?:\\sw\\|\\s_\\)*\\)[ \t]+\\(extends\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\(?:\\sw\\|\\s_\\)*\\)[ \t]*;"
  "Value for `antlr-grammar-header-regexp' when using ANTLR v2.")

(defvar antlr-ruleref-assign-regexp "\\(\\sw+\\)[ \t]*\\(\\+?=\\)?"
  "Regexp matching rule references or their optional labels.
If the second regexp group does not match, the first regexp group
matches a rule reference, which is highlighted with face `antlr-tokenref'
for token rules, and face `antlr-ruleref' for other rules.

If there is no third regexp group or it does not match, the first
regexp group matches a rule label, which is highlighted with face
`font-lock-variable-name-face'.

The value might be tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-v2-ruleref-assign-regexp "\\(\\sw+\\)[ \t]*\\([:]\\|\\(=\\)\\)?"
  "Value for `antlr-ruleref-assign-regexp' when using ANTLR v2.")

(defvar antlr-action-font-lock-keywords nil
  "Font Lock keywords used for the actions in the grammar.
The value should be like the first element of `font-lock-defaults.
See also `antlr-font-lock-maximum-decoration'.
The value might be language-dependent, see `antlr-language-variables'.")

(defvar antlr-java-action-font-lock-keywords
  '(antlr-no-action-keywords
    java-font-lock-keywords-1 java-font-lock-keywords-2
    java-font-lock-keywords-3)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-java'.")

(defvar antlr-action-scope-names '("lexer" "parser" "treeparser")
  "Valid ANTLR action scope names.")

(defvar antlr-action-names t
  "Valid ANTLR action names.
This is a string list or t, which means that any name is valid.
The value might be language-dependent, see `antlr-language-variables'.")

;; see $(ANTLR3)/tool/src/main/java/org/antlr/codegen/$(LANGUAGE)Target.java-isValidActionScope()
;; or $(ANTLR3.JAR)/antlr3-jar/org/antlr/codegen/templates/, fine-grep for "actions\."
(defvar antlr-java-action-names
  '("init" "after" "header" "members" "rulecatch" "synpredgate")
  "Valid ANTLR action names in Java.
Value for `antlr-action-names' when using language `antlr-java'.")

;; FIXME: We usually call such variables "foo-predicate".
(defvar antlr-token-identifier-p #'antlr-upcase-p
  "Function for syntax highlighting to distinguish token refs from rule refs.
Function is called with the first character of the identifier; it should
return non-nil if the identifier is a token reference.")


;;;===========================================================================
;;;  general font-lock
;;;===========================================================================

(defcustom antlr-font-lock-maximum-decoration 'inherit
  "The maximum decoration level for fontifying actions.
Value `none' means, do not fontify actions, just normal grammar
code according to `antlr-font-lock-additional-keywords' and
`antlr-font-lock-late-keywords'.  Value `inherit' means, use
value of `font-lock-maximum-decoration'.  Any other value is
interpreted as in `font-lock-maximum-decoration' with no level-0
fontification, see `antlr-font-lock-keywords-alist'.

While calculating the decoration level for actions, `major-mode' is
bound to the value of `antlr-action-mode'.  For example, with value
  ((java-mode . 2) (c++-mode .  0))
Java actions are fontified with level 2 and C++ actions are not
fontified at all."
  :type '(choice (const :tag "None" none)
		 (const :tag "Inherit" inherit)
		 (const :tag "Default" nil)
		 (const :tag "Maximum" t)
		 (integer :tag "Level" 1)
		 (repeat :menu-tag "Mode specific" :tag "Mode specific"
			 :value ((t . t))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "All" t)
				      (symbol :tag "Name"))
			       (radio :tag "Decoration"
				      (const :tag "Default" nil)
				      (const :tag "Maximum" t)
				      (integer :tag "Level" 1))))))

(defconst antlr-no-action-keywords nil
  ;; Using nil directly won't work (would use highest level, see
  ;; `font-lock-choose-keywords'), but a non-symbol, i.e., (list), at `car'
  ;; would break Emacs-21.0:
  "Empty font-lock keywords for actions.
Do not change the value of this constant.")

(defface antlr-default '((t nil))
  "Face to prevent strings from language dependent highlighting.
Do not change.")

(defface antlr-keyword
  '((t :inherit font-lock-keyword-face))
  "ANTLR keywords.")

(defface antlr-syntax
  '((t :inherit font-lock-keyword-face))
  "ANTLR syntax symbols for AST creation: !, ^, ->.")

(defface antlr-action
  '((t :inherit font-lock-builtin-face))
  "ANTLR action names: @ActionName, @ActionScope::ActionName.")

(defface antlr-ruledef
  '((t :inherit font-lock-function-name-face))
  "ANTLR parser and treeparser rule symbols (definition).")

(defface antlr-tokendef
  '((t :inherit font-lock-function-name-face))
  "ANTLR scanner rule symbols (definition).")

(defface antlr-ruleref
  '((t :inherit font-lock-type-face))
  "ANTLR parser and treeparser rule symbols (usage).")

(defface antlr-tokenref
  '((t :inherit font-lock-constant-face))
  "ANTLR scanner rule symbols (usage).")

(defface antlr-symbol
  '((t :inherit font-lock-variable-name-face))
  "ANTLR symbols (definition and usage) for things other than rules.
Used for grammars, v3 scopes and v4 modes.")

(defface antlr-literal
  '((t :inherit font-lock-string-face :weight bold))
  "ANTLR special literal tokens.
It is used to highlight strings matched by the first regexp group of
`antlr-font-lock-literal-regexp'.")

(defface antlr-attribute '((t :inherit font-lock-preprocessor-face))
  "ANTLR references to attributes within actions.")

(defvar antlr-font-lock-late-keywords
  ;; The tokens are already fontified as string/docstrings.  The extra
  ;; fontification of literals must come after the fontification from cc-mode;
  ;; otherwise `c-font-lock-invalid-string' fontifies the final doublequote of
  ;; the last literal in a line with red (warning) - for whatever reason.
  `((,(lambda (limit)                   ; v3, v4: literals are only '...'
        (if antlr-font-lock-literal-regexp
            (antlr-re-search-forward antlr-font-lock-literal-regexp limit)))
     (2 'antlr-literal t))
    (,(lambda (limit)
        (antlr-re-search-forward "^\\(\\sw+\\)" limit))
     (1 (if (funcall antlr-token-identifier-p (char-after (match-beginning 0)))
            'antlr-tokendef
          'antlr-ruledef)
        t))
    (,(lambda (limit)
        (antlr-re-search-forward antlr-grammar-header-regexp limit))
     (1 'antlr-keyword t)
     (2 'antlr-symbol t)
     (3 'antlr-keyword t t)
     (4 (if (member (match-string-no-properties 4) '("Lexer" "Parser" "TreeParser"))
            'antlr-keyword
          'font-lock-type-face)
        t t))
    (,(lambda (limit)
        (antlr-re-search-forward
         "\\<\\(header\\|options\\|tokens\\|channels\\|exception\\|catch\\|finally\\|returns\\|throws\\|import\\|locals\\)\\>"
         limit))
     (1 'antlr-keyword t))
    (,(lambda (limit)
        (when antlr-font-lock-symbol-regexp
          (antlr-re-search-forward antlr-font-lock-symbol-regexp limit)))
     (1 'antlr-keyword t)
     (2 'antlr-symbol t t))
    (,(lambda (limit)
        (antlr-re-search-forward
         "^\\(private\\|public\\|protected\\|fragment\\)\\>[ \t]*\\(\\sw+\\)?"
         limit))
     (1 'antlr-keyword t)
     (2 (if (funcall antlr-token-identifier-p (char-after (match-beginning 2)))
            'antlr-tokendef
          'antlr-ruledef)
        t t))
    (,(lambda (limit)                   ; v3, v4
        (antlr-re-search-forward "@\\([A-Za-z\300-\326\330-\337_]\\sw*\\)\\(?:::\\([A-Za-z\300-\326\330-\337_]\\sw*\\)\\)?" limit))
     (1 (antlr-font-lock-checked-face (if (match-beginning 2)
                                          antlr-action-scope-names
                                        antlr-action-names)
                                      1 'antlr-action)
        t)
     (2 (antlr-font-lock-checked-face antlr-action-names 2 'antlr-action)
        t t))
    (,(lambda (limit)
        (and antlr-font-lock-syntax-spec
             (antlr-re-search-forward (car antlr-font-lock-syntax-spec) limit)))
      (1 'antlr-syntax t)))
  "Late font-lock keywords for ANTLR's normal grammar code.
See `antlr-font-lock-keywords-alist' for the keywords of actions.")

(defvar antlr-font-lock-additional-keywords
  `((,(lambda (limit)
        (and antlr-font-lock-attribute-regexp
             (re-search-forward antlr-font-lock-attribute-regexp limit 'limit)))
     (1 'antlr-attribute))
    (,(lambda (limit)
        ;; v2: v:ruleref v:"literal", v=ruleref (no highlighting), v3: v=ruleref
        (antlr-re-search-forward antlr-ruleref-assign-regexp limit))
     (1 (if (match-beginning 2)
            (if (match-beginning 3)
                'antlr-default
              'font-lock-variable-name-face) ; yes, same as vars in [...]
          (if (funcall antlr-token-identifier-p (char-after (match-beginning 1)))
              'antlr-tokenref
            'antlr-ruleref)))
     (2 'antlr-default nil t))
    (,(lambda (limit)
        (antlr-re-search-forward antlr-font-lock-negation-regexp limit))
     (1 'font-lock-negation-char-face t t)))
"Early font-lock keywords for ANTLR's normal grammar code.
See `antlr-font-lock-keywords-alist' for the keywords of actions.")

(defvar antlr-font-lock-defaults
  '(antlr-font-lock-keywords
    nil nil ((?_ . "w")))
  "Font-lock defaults used for ANTLR syntax highlighting.")


;;;===========================================================================
;;;  Internal variables
;;;===========================================================================

(defvar antlr-mode-hook nil
  "Hook called by `antlr-mode'.")

(defvar antlr-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    st)
  "Syntax table used in `antlr-mode' buffers.")

(defvar antlr-mode-abbrev-table nil
  "Abbreviation table used in `antlr-mode' buffers.")
(define-abbrev-table 'antlr-mode-abbrev-table ())


;;;;##########################################################################
;;;;  The Code
;;;;##########################################################################



;;;===========================================================================
;;;  Context cache
;;;===========================================================================

(defun antlr-syntactic-context (&optional ppss)
  "Return some syntactic context information.
Return `string' if point is within a string, `block-comment' or
`comment' if point is within a comment or the depth within all
parenthesis-syntax delimiters at point otherwise.
WARNING: this may alter `match-data'.
Optional argument PPSS"  ; TODO: warning this valid?
  ;; does not work for negative depth
  (or ppss (setq ppss (syntax-ppss)))
  (cond ((nth 3 ppss) 'string)
        ((nth 4 ppss) 'comment)
        (t
         (let ((poss (nth 9 ppss)))        ; TODO Emacs: syntax-ppss-open-positions
           (while (and poss (memq (char-after (car poss)) '(nil ?\()))
             (setq poss (cdr poss)))
           (and poss (length poss)))))) ; depth if inside {} or []


;;;===========================================================================
;;;  Miscellaneous functions
;;;===========================================================================

(defun antlr-upcase-p (char)
  "Non-nil, if CHAR is an uppercase character (if CHAR was a char)."
  ;; (get-char-code-property char 'lowercase)
  (not (eq (downcase char) char)))

(defun antlr-re-search-forward (regexp bound)
  "Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.  Return
nil if no occurrence was found.  Do not search within comments, strings
and actions/semantic predicates.  BOUND bounds the search; it is a
buffer position.  See also the functions `match-beginning', `match-end'
and `replace-match'."
  (let ((continue t))
    (while (and (re-search-forward regexp bound 'limit)
		(save-match-data
		  (or (antlr-syntactic-context) (setq continue nil)))))
    (if continue nil (point))))

(defsubst antlr-search-result (line-regexp)
  "Return `point' if last search is valid, or nil otherwise.
The search is not considered valid if point is inside actions, comments
or strings, or if the beginning of the current line matches LINE-REGEXP
if that is non-nil."
  (unless (antlr-syntactic-context)
    (if (and line-regexp
             (save-excursion
               (beginning-of-line)
               (looking-at line-regexp))
             (<= (point) (match-end 0) (1+ (point))))
        nil
      (point))))

(defun antlr-search-forward (string &optional line-regexp)
  "Search forward from point for STRING.
Set point to the end of the occurrence found, and return point.  Return
nil if no occurrence was found.  Do not search within comments, strings
and actions/semantic predicates.
See function `antlr-search-result' for the optional arg LINE-REGEXP."
  (let ((result nil))
    (while (and (null result) (search-forward string nil 'limit))
      (setq result (antlr-search-result line-regexp)))
    result))

(defun antlr-search-backward (string &optional line-regexp)
  "Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
Return nil if no occurrence was found.  Do not search within comments,
strings and actions/semantic predicates.
See function `antlr-search-result' for the optional arg LINE-REGEXP."
  (let ((result nil))
    (while (and (null result) (search-backward string nil 'limit))
      (setq result (antlr-search-result line-regexp)))
    result))

(defsubst antlr-skip-sexps (count)
  "Skip the next COUNT balanced expressions and the comments after it.
Return position before the comments after the last expression."
  (goto-char (or (ignore-errors (scan-sexps (point) count)) (point-max)))
  (prog1 (point)
    (antlr-c-forward-sws)))

(defun antlr-syntax-propertize-wholerule (start end)
  ;; checkdoc-params: (start end)
  "Function used to properly highlight ANTLR v4 charsets.
This function is used in `syntax-propertize-extend-region-functions' to
make sure that the propertized region starts at the beginning of a rule."
  (goto-char start)
  (beginning-of-line)
  (while (if (bobp) nil (looking-at "[ \t\n}]\\|@init\\_>\\|options\\_>"))
    (beginning-of-line 0))
  ;; no need to find the real end of a rule... end-of-line is good
  (cons (point)
        (progn (goto-char end)
               (if (bolp) (point) (line-beginning-position 2)))))

;; font-lock.el and/or syntax.el should say something about the use of
;; `syntax-ppss' when `font-lock-syntax-table' is set - we should not use the
;; same cache for calls inside and outside font-lock, don't we?

(defun antlr-syntax-propertize-charsets (start end)
  ;; checkdoc-params: (start end)
  "Function used to properly highlight ANTLR v4 charsets.
This function is as value for `syntax-propertize-function' and makes
sure that charsets like [a-z] in token rule bodies are considered
literals."
  (goto-char start)
  (let ((mode (if (bolp) :start :next)))
    (while (< (point) end)
      (let ((context (antlr-syntactic-context)))
        (cond ((numberp context)
               (goto-char (or (ignore-errors (scan-lists (point) 1 context))
                              end)))
              ((eq mode :start)         ; TODO: only if context = 0
               (when (looking-at "fragment\\_>")
                 (forward-char 8)
                 (skip-chars-forward " \t\n"))
               (setq mode (if (antlr-upcase-p (char-after))
                              :scanner
                            (if (eq (char-syntax (char-after)) ?w)
                                :end
                              :next))))
              ((eq mode :end)
               (skip-chars-forward "^;" end)
               (while (and (< (point) end) (antlr-syntactic-context))
                 (forward-char)
                 (skip-chars-forward "^;" end))
               (setq mode :next))
              ((eq mode :next)
               (beginning-of-line 2)
               (while (and (< (point) end) (looking-at "[ \t\n@}]"))
                 (beginning-of-line 2))
               (setq mode :start))
              ((eq mode :scanner)
               (skip-chars-forward "^:;" end)
               (while (and (< (point) end) (antlr-syntactic-context))
                 (forward-char)
                 (skip-chars-forward "^:;" end))
               (if (eq (char-after) ?:)
                   (if (eq (char-after (1+ (point))) ?:)
                       (forward-char 2) ; "::" for namespace
                     (setq mode :charset))
                 (setq mode :next)))
              ((eq mode :charset)
               ;; LEXER_CHAR_SET :
               ;;     '[' ( '\\' ~('\r'|'\n') |	~('\r'|'\n'|'\\'|']') )* ']'
               (skip-chars-forward "^;[" end)
               (while (and (< (point) end) (antlr-syntactic-context))
                 (forward-char)
                 (skip-chars-forward "^;[" end))
               (if (not (eq (char-after) ?\[))
                   (setq mode :next)
                 (put-text-property (point) (progn (forward-char) (point))
                                    'syntax-table
                                    (eval-when-compile
                                      (string-to-syntax "|")))
                 (let (char (esc nil))
                   (while (and (setq char (char-after))
                               (not (eq char ?\n))
                               (or esc (not (eq char ?\]))))
                     (setq esc (if esc nil (eq char ?\\)))
                     (forward-char)))
                 (put-text-property (point) (1+ (point))
                                    'syntax-table
                                    (eval-when-compile
                                      (string-to-syntax "|"))))))))))

(defun antlr-syntax-propertize-template-literals (start end)
  ;; checkdoc-params: (start end)
  "Function used to properly highlight ANTLR v3 template literals.
This function is as value for `syntax-propertize-function' and makes
sure that templates like <<...>> outside actions, strings or comments
are considered literals."
  (goto-char start)
  (while (search-forward "<<" end t)
    (let ((context (antlr-syntactic-context)))
      (if context
          (when (numberp context)
            (goto-char (min (or (ignore-errors (scan-lists (point) 1 context))
                                end)
                            end)))
        (let ((pos (- (point) 2)))
          ;; put the text property on the inner "<>", since multi-line is set
          ;; to be ok with `c-multiline-string-start-char'
          (put-text-property (1+ pos) (point) 'syntax-table
                             (eval-when-compile
                               (string-to-syntax "|")))
          (when (search-forward ">>" end 'move)
            (put-text-property (- (point) 2) (1- (point)) 'syntax-table
                               (eval-when-compile
                                 (string-to-syntax "|"))))
          (put-text-property pos (point) 'syntax-multiline t))))))


;;;===========================================================================
;;;  font-lock
;;;===========================================================================

(defun antlr-font-lock-keywords ()
  "Return font-lock keywords for current buffer.
See `antlr-font-lock-additional-keywords', `antlr-language' and
`antlr-font-lock-maximum-decoration'."
  (append antlr-font-lock-additional-keywords
          (unless (eq antlr-font-lock-maximum-decoration 'none)
            (let* ((major-mode antlr-action-mode) ;#dynamic
                   (level (font-lock-value-in-major-mode
                           (if (eq antlr-font-lock-maximum-decoration 'inherit)
                               font-lock-maximum-decoration
                             antlr-font-lock-maximum-decoration))))
              (font-lock-eval-keywords
               (font-lock-choose-keywords antlr-action-font-lock-keywords
                                          level))))
          antlr-font-lock-late-keywords))

(defun antlr-font-lock-checked-face (strings group face) ; checkdoc-order: nil
  "Return font-lock face for regexp group GROUP.
If the matched string is an element of STRINGS (or STRINGS is not a list),
return FACE, otherwise return `font-lock-warning-face'."
  (if (if (consp strings) (member (match-string-no-properties group) strings) strings)
      face
    'font-lock-warning-face))


;;;===========================================================================
;;;  imenu support
;;;===========================================================================

;; Actually, issues in the "Index" (at least with sorted entries) menu are
;; imenu-induced.  If entries are missing in the menu (as are sometimes for me
;; on Emacs-25.1.1), try M-x imenu RET -> you see all.  This might not be the
;; case anymore with the reordering of `imenu-add-to-menubar'...
(defvar antlr-do-syntax-propertize (< emacs-major-version 25)
  "Whether \\[antlr-mode] runs `syntax-propertize' on the complete buffer.
Running it explicitly at the beginning of the mode might be
necessary for a correct Index menu and motion commands.")

(defun antlr-grammar-tokens ()
  "Return alist for tokens defined in current buffer."
  (save-excursion (antlr-imenu-create-index-function 'upcase)))

;; TODO: version dependent?
(defun antlr-imenu-create-index-function (&optional refs-only)
  "Return imenu index-alist for ANTLR grammar files.
IF REFS-ONLY is non-nil, just return alist with ref names,
with value `upcase', only return alist with tokenref names."
  (let ((items nil)
	(classes nil)
	(continue t))
    ;; Using `imenu-progress-message' would require imenu for compilation, but
    ;; nobody is missing these messages.  The generic imenu function searches
    ;; backward, which is slower and more likely not to work during editing.
    (goto-char (point-min))
    (antlr-skip-file-prelude t)
    (while continue
      (if (looking-at "\\(class\\|lexer[ \t]+grammar\\|parser[ \t]+grammar\\|tree[ \t]+grammar\\|grammar\\|mode\\|import\\)[ \t]+\\([A-Za-z\300-\326\330-\337]\\(?:\\sw\\|\\s_\\)*\\)") ; TODO: import is (hopefully) temp
          (and (not refs-only)
               (memq (char-after (match-beginning 1)) '(?c ?m)) ;class, mode
               (push (cons (match-string-no-properties 2)
                           (if imenu-use-markers
                               (copy-marker (match-beginning 2))
                             (match-beginning 2)))
                     classes))
        (if (looking-at "p\\(ublic\\|rotected\\|rivate\\)\\_>\\|fragment\\_>")
            (antlr-skip-sexps 1))
        (when (looking-at "\\(?:\\sw\\|\\s_\\)+")
          (when (or (not (eq refs-only 'upcase))
                    (antlr-upcase-p (char-after (point))))
            (push (cons (match-string-no-properties 0)
                        (if (and imenu-use-markers (not refs-only))
                            (copy-marker (match-beginning 0))
                          (match-beginning 0)))
                  items))))
      (if (setq continue (antlr-search-forward ";" antlr-skip-line-regexp))
          (antlr-skip-rule-postlude t)))
    (if classes
	(cons (cons (if (eq antlr-tool-version 'antlr-v2) "Classes" "Modes")
                    (nreverse classes))
              (nreverse items))
      (nreverse items))))


;;;===========================================================================
;;;  Parse grammar files (internal functions)
;;;===========================================================================

;; --- simplified v2 grammar -------------------------------------------------
;; file: ("header" STRING? ACTION)? OPTIONS? ACTION? class*
;; class: "class" ID     // moved preamble action to file rule
;;        ("extends" ("Lexer"|"Parser"|"TreeParser") ID?)?  ";"
;;        OPTIONS? TOKENS? ACTION? rule*
;; rule: ("protected"|"public"|"private")? ID "!"?
;;       (ARGS)? ('returns' ARGS)? ('throws' IDs )? OPTIONS? ACTION?
;;       ":" alts ";" ("exception" ARGS? ("catch" ARGS ACTION)* )*

;; --- simplified v3/v4 grammar ----------------------------------------------
;; grammar: ('lexer'|'parser'|'tree')? 'grammar' id ';'
;;          < OPTIONS? TOKENS? (grammar3only|grammar4only) action* >
;;          rule* ('mode' ID ';' rule* )*          // lexer MODE is v4 only
;; grammar3only: ('scope' id ACTION)*              // strict sequence in <...>
;; grammar4only: ('import' IDs ';')? ('channels' ACTION)? // any sequence in <...>
;; action: '@' (ID '::')? ID ACTION
;; rule: ('protected'|'public'|'private'|'fragment')? ID '!'?
;;       (ARGS)? ('returns' ARGS)? ('throws' IDs )? ('locals' ARGS)?
;;       < OPTIONS? scope3only action* >
;;       ':' alts ';' ('catch' ARGS ACTION)* ('finally' ACTION)?
;; scope3only: ('scope' ACTION)? ('scope' IDs ';' )?

(eval-and-compile
  (defconst antlr-rule-postlude-skip-alist--const ; const for eval-when-compile safety
    '(("exception" 1 . t) ("import" antlr-skip-import-statement)
      ("options" 2) ("tokens" 2) ("finally" 2) ("channels" 2)
      ("catch" 3) ("scope" 3))
    "Constant for `antlr-rule-postlude-skip-alist'."))

(defvar antlr-rule-postlude-skip-alist antlr-rule-postlude-skip-alist--const
  "Alist of keywords after the ';' which still belong to the grammar rule.
Each element looks like
  (KEYWORD FUNCTION ARGS...)
or
  (KEYWORD SEXPS-COUNT OPTIONALP)

After the ';' ending a rule body, function `antlr-skip-rule-postlude'
skips whitespace and comments, and check the text after point against
`antlr-rule-postlude-skip-regexp'.  While there is a match,

- if regexp group 1 has not been matched: we set point the end of the,
  match, and skip the next sexpr, whitespace and comments

- if the string matched by regexp group 1 is not the car of an element
  of `antlr-rule-postlude-skip-alist': we skip one sexpr - which should
  move point to the end of the whole match,

- when the corresponding element of `antlr-rule-postlude-skip-alist'
  is of the first form, we call FUNCTION with arguments ARGS - this
  the function should return the position after a match and move
  point to the non whitespace/comment position after that

- when the corresponding element of `antlr-rule-postlude-skip-alist'
  is of the second form, we skip SEXPS-COUNT balanced expression
  (including the keyword itself)

The value might be tool-dependent, see `antlr-tool-version-variables'.")

(defvar antlr-rule-postlude-skip-regexp
  (eval-when-compile
    (concat (regexp-opt (mapcar #'car antlr-rule-postlude-skip-alist--const) t)
            "\\_>\\|@[A-Za-z\300-\326\330-\337_]\\(?:\\sw\\|\\s_\\)*\\(?:::[A-Za-z\300-\326\330-\337_]\\(?:\\sw\\|\\s_\\)*\\)?"))
  "Regexp matching things after ';' which still belong to the grammar rule.
See `antlr-rule-postlude-skip-alist' for details.
The value might be tool-dependent, see `antlr-tool-version-variables'.")

(defun antlr-skip-rule-postlude (skip-comment)
  "Skip the postlude of a definition, i.e. everything after `;'.
Definitions are rules, grammar/class and v4 mode definition.  If
SKIP-COMMENT is non-nil, also skip the whitespace and comment
after that part.  See `antlr-rule-postlude-skip-alist'.

Point is assumed to be after the `;'.  Always return end position
before trailing whitespaces and comments"
  (let ((pos (point)))
    (antlr-c-forward-sws)
    (while (looking-at antlr-rule-postlude-skip-regexp)
      (if (match-end 1)
          (let ((skip (cdr (assoc (match-string-no-properties 1)
                                  antlr-rule-postlude-skip-alist))))
            (if (functionp (car skip))
                (setq pos (apply (car skip) (cdr skip)))
              (setq pos (antlr-skip-sexps (or (car skip) 1)))
              (when (and (cdr skip) (eq (char-after) ?\[))
                (setq pos (antlr-skip-sexps 1)))))
        (goto-char (match-end 0))       ; to end of @action / @scope::action
        (antlr-c-forward-sws)
        (setq pos (antlr-skip-sexps 1))))
    (if (eq (char-after) ?\{) (setq pos (antlr-skip-sexps 1))) ; v2
    (or skip-comment (goto-char pos))))

(defun antlr-skip-file-prelude (skip-comment)
  "Skip the file prelude: the header and file options.
If SKIP-COMMENT is non-nil, also skip the comment after that part.
Return the start position of the file prelude.

Hack: if SKIP-COMMENT is `header-only' only skip header and return
position before the comment after the header."
  (let* ((pos (point))                  ; should be (point-min)
	 (pos0 pos))
    (antlr-c-forward-sws)
    (if skip-comment (setq pos0 (point)))
    (while (looking-at "header\\_>[ \t]*\\(\"\\)?")
      (setq pos (antlr-skip-sexps (if (match-beginning 1) 3 2))))
    (if (eq skip-comment 'header-only)	; a hack...
	pos
      (when (looking-at "options\\_>")
	(setq pos (antlr-skip-sexps 2)))
      (if (eq (char-after) ?\{) (setq pos (antlr-skip-sexps 1)))
      (or skip-comment (goto-char pos))
      pos0)))

(defun antlr-next-rule (arg skip-comment)
  "Move forward to next end of rule.  Do it ARG many times.
A grammar/class definition and the file prelude of Antlr v2
grammars are also considered as a rule.  Negative argument ARG
means move back to ARGth preceding end of rule.  The behavior is
not defined when ARG is zero.  If SKIP-COMMENT is non-nil, move
to beginning of the rule."
  ;; PRE: ARG<>0
  (let ((pos (point))
	(beg (point)))
    ;; first look whether point is in rule postlude
    (if (antlr-search-backward ";" antlr-skip-line-regexp)
	(progn
	  (setq beg (point))
	  (forward-char)
	  (antlr-skip-rule-postlude skip-comment))
      (antlr-skip-file-prelude skip-comment))
    (if (< arg 0)
	(unless (and (< (point) pos) (zerop (cl-incf arg)))
	  ;; if we have moved backward, we already moved one defun backward
	  (goto-char beg)		; rewind (to ";" / point)
	  (while (and arg (<= (cl-incf arg) 0))
	    (if (antlr-search-backward ";" antlr-skip-line-regexp)
		(setq beg (point))
	      (when (>= arg -1)
		;; try file prelude:
		(setq pos (antlr-skip-file-prelude skip-comment)) ; header pos
		(if (zerop arg)
		    (if (>= (point) beg)
			(goto-char (if (>= pos beg) (point-min) pos)))
		  (goto-char (if (or (>= (point) beg) (= (point) pos))
				 (point-min) pos))))
	      (setq arg nil)))
	  (when arg			; always found a ";"
	    (forward-char)
	    (antlr-skip-rule-postlude skip-comment)))
      (if (<= (point) pos)		; moved backward?
	  (goto-char pos)		; rewind
	(decf arg))			; already moved one defun forward
      (unless (zerop arg)
	(while (>= (decf arg) 0)
	  (antlr-search-forward ";" antlr-skip-line-regexp))
	(antlr-skip-rule-postlude skip-comment)))))

(defun antlr-outside-rule-p ()
  "Non-nil if point is outside a grammar rule.
Move to the beginning of the current rule if point is inside a rule."
  (let ((pos (point)))
    (antlr-next-rule -1 nil)            ; to end of previous rule
    (let ((between (or (bobp) (< (point) pos))))
      (antlr-c-forward-sws)
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
    (not (antlr-outside-rule-p))))

(defun antlr-end-of-rule (&optional arg)
  "Move forward to next/end of rule.  Do it ARG [default: 1] many times.
A grammar/class header and the file prelude are also considered a
rule.

If `antlr-end-of-defun-is-next' is nil, move to next end of rule,
i.e. the end of the current rule with ARG = 1.  Otherwise move
forward to the ARGth next rule.

Negative argument ARG means move back to ARGth preceding end of
rule.  If ARG is zero, run `antlr-end-of-body'."
  (interactive "^p")
  ;; yes, there is a variable `end-of-defun-function', but `end-of-defun' does
  ;; far too much around the funcall of that variable (Emacs-24.4)
  (if (zerop arg)
      (antlr-end-of-body)
    (antlr-next-rule arg (and antlr-end-of-defun-is-next (> arg 0)))))

(defun antlr-beginning-of-rule (&optional arg)
  "Move backward to preceding beginning of rule.  Do it ARG many times.
A grammar/class header and the file prelude are also considered a
rule.

Negative argument ARG means move forward to abs(ARG)th next rule:
beginning of rule if `antlr-end-of-defun-is-next' is nil, and end
of rule otherwise.

If ARG is zero, run `antlr-beginning-of-body'."
  (interactive "^p")
  (if (zerop arg)
      (antlr-beginning-of-body)
    (antlr-next-rule (- arg) (if antlr-end-of-defun-is-next (> arg 0) t))))

(defun antlr-end-of-body (&optional msg)
  "Move to position after the `;' of the current rule.
A grammar class header is also considered as a rule.  With optional
prefix arg MSG, move to `:'."
  (interactive "^")
  (let ((orig (point)))
    (if (antlr-outside-rule-p)
        (error "Outside an ANTLR rule"))
    (let ((bor (point)))              ; beginning of current rule
      (when (< (antlr-skip-file-prelude t) (point))
        ;; Yes, we are in the file prelude
        (goto-char orig)
        (error (or msg "The file prelude is without `;'")))
      (antlr-search-forward ";" antlr-skip-line-regexp)
      (when msg
        (when (< (point)
                 (progn (goto-char bor)
                        (or (antlr-search-forward antlr-rule-body-start-op)
                            (point-max))))
          (goto-char orig)
          (error msg))
        (antlr-c-forward-sws)))))

(defun antlr-beginning-of-body ()
  "Move to the first element after the `:' of the current rule."
  (interactive "^")
  (antlr-end-of-body (if (eq antlr-tool-version 'antlr-v2)
                         "Class definitions and the file prelude are without `:'"
                       "Grammar and mode definitions are without `:'")))


;;;===========================================================================
;;;  Literal normalization, Hide Actions
;;;===========================================================================

(defun antlr-downcase-literals (&optional transform)
  "Convert all literals in buffer to lower case.
If non-nil, TRANSFORM is used on literals instead of `downcase-region'."
  (interactive)
  (or transform (setq transform #'downcase-region))
  (let ((literals 0))
    (save-excursion
      (goto-char (point-min))
      (while (antlr-re-search-forward "\"\\(\\sw\\(\\sw\\|\\s_\\|-\\)*\\)\"" nil) ; TODO: '...'
        (funcall transform (match-beginning 0) (match-end 0))
        (cl-incf literals)))
    (message "Transformed %d literals" literals)))

(defun antlr-upcase-literals ()
  "Convert all literals in buffer to upper case."
  (interactive)
  (antlr-downcase-literals #'upcase-region))

;; TODO: `antlr-hide-actions' should probably be a minor mode like
;; `hide-ifdef-mode'.  Whether to exclude arguments (of limited use) should be
;; controlled by `antlr-action-visibility' (negative value)
(defun antlr-hide-actions (arg &optional silent)
  "Hide or unhide all actions in buffer.
Hide all actions including arguments in brackets if ARG is 1 or if
called interactively without prefix argument.  Hide all actions
excluding arguments in brackets if ARG is 2 or higher.  Unhide all
actions if ARG is 0 or negative.  See `antlr-action-visibility'.

Display a message unless optional argument SILENT is non-nil."
  (interactive "p")
  (with-silent-modifications
    (if (> arg 0)
	(let ((regexp (if (= arg 1) "[]}]" "}"))
	      (diff (and antlr-action-visibility
			 (+ (max antlr-action-visibility 0) 2))))
	  (antlr-hide-actions 0 t)
	  (save-excursion
	    (goto-char (point-min))
            (while (antlr-re-search-forward regexp nil)
              (let ((beg (ignore-errors (scan-sexps (point) -1))))
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
                                         '(invisible t intangible t)))))))
	  (or silent
	      (message "Hide all actions (%s arguments)...done"
		       (if (= arg 1) "including" "excluding"))))
      (remove-text-properties (point-min) (point-max)
			      '(invisible nil intangible nil))
      (or silent
	  (message "Unhide all actions (including arguments)...done")))))


;;;===========================================================================
;;;  Insert option: command
;;;===========================================================================

(defun antlr-insert-option (level option &optional location)
  "Insert file/grammar/rule/subrule option near point.
LEVEL determines option kind to insert: 1=file, 2=grammar, 3=rule,
4=subrule.  OPTION is a string with the name of the option to insert.
LOCATION can be specified for not calling `antlr-option-kind' twice.

Inserting an option with this command works as follows:

 1. When called interactively, LEVEL is determined by the prefix
    argument or automatically deduced without prefix argument.
 2. Signal an error if no option of that level could be inserted, e.g.,
    if the buffer is read-only, the option area is outside the visible
    part of the buffer or a subrule/rule option should be inserted with
    point outside a subrule/rule.
 3. When called interactively, OPTION is read from the minibuffer with
    completion over the known options of the given LEVEL.
 4. Ask user for confirmation if the given OPTION does not seem to be a
    valid option to insert into the current file.
 5. Find a correct position to insert the option.
 6. Depending on the option, insert it the following way (inserting an
    option also means inserting the option section if necessary):
     - Insert the option and let user insert the value at point.
     - Read a value (with completion) from the minibuffer, using a
       previous value as initial contents, and insert option with value.
 7. Final action depending on the option.  For example, set the language
    according to a newly inserted language option.

The name of all options with a specification for their values are stored
in `antlr-options-alists' which depends on `antlr-tool-version'.

If the option already exists inside the visible part of the buffer, this
command can be used to change the value of that option.  Otherwise, find
a correct position where the option can be inserted near point.

The search for a correct position is as follows:

  * If search is within an area where options can be inserted, use the
    position of point.  Inside the options section and if point is in
    the middle of an option definition, skip the rest of it.
  * If an options section already exists, insert the options at the end.
    If only the beginning of the area is visible, insert at the
    beginning.
  * Otherwise, find the position where an options section can be
    inserted and insert a new section before any comments.  If the
    position before the comments is not visible, insert the new section
    after the comments.

This function also inserts \"options {...}\" and the \":\" if necessary,
see `antlr-options-auto-colon'.  See also `antlr-options-assign-string'.

This command might also set the mark like \\[set-mark-command] does, see
`antlr-options-push-mark'."
  (interactive (antlr-insert-option-interactive current-prefix-arg))
  (barf-if-buffer-read-only)
  (or location (setq location (cdr (antlr-option-kind level))))
  (cond ((null level)
         ;; TODO: better msg if there is (currently) no such option
	 (error "Cannot deduce what kind of option to insert"))
	((atom location)
	 (error "Cannot insert any %s options around here"
		(elt antlr-options-headings (1- level)))))
  (let ((area (car location))
	(place (cdr location)))
    (cond ((null place)		; invisible
	   (error (if area
		      "Invisible %s options, use %s to make them visible"
		    "Invisible area for %s options, use %s to make it visible")
		  (elt antlr-options-headings (1- level))
		  (substitute-command-keys "\\[widen]")))
	  ((null area)			; without option part
	   (antlr-insert-option-do level option nil
				   (null (cdr place))
				   (car place)))
	  ((save-excursion		; with option part, option visible
	     (goto-char (max (point-min) (car area)))
	     (re-search-forward (concat "\\(^\\|;\\)[ \t]*\\(\\<"
					(regexp-quote option)
					"\\_>\\)[ \t\n]*\\(\\(=[ \t]?\\)[ \t]*\\(\\(\\sw\\|\\s_\\)+\\|\"\\([^\n\"\\]\\|[\\][^\n]\\)*\"\\)?\\)?")
				;; 2=name, 3=4+5, 4="=", 5=value
				(min (point-max) (cdr area))
				t))
	   (antlr-insert-option-do level option
				   (cons (or (match-beginning 5)
					     (match-beginning 3))
					 (match-end 5))
				   (and (null (cdr place)) area)
				   (or (match-beginning 5)
				       (match-end 4)
				       (match-end 2))))
	  (t				; with option part, option not yet
	   (antlr-insert-option-do level option t
				   (and (null (cdr place)) area)
				   (car place))))))

(defun antlr-insert-option-interactive (arg)
  "Interactive specification for `antlr-insert-option'.
Use `current-prefix-arg' for ARG.  Return \(LEVEL OPTION LOCATION)."
  (barf-if-buffer-read-only)
  (if arg (setq arg (prefix-numeric-value arg)))
  (unless (memq arg '(nil 1 2 3 4))
    (error "Valid prefix args: no=auto, 1=file, 2=grammar, 3=rule, 4=subrule"))
  (let* ((kind (antlr-option-kind arg))
	 (level (car kind)))
    (if (atom (cdr kind))
	(list level nil (cdr kind))
      (let* ((table (elt antlr-options-alists (1- level)))
	     (completion-ignore-case t)	;dynamic
	     (input (completing-read (format "Insert %s option: "
					     (elt antlr-options-headings
						  (1- level)))
				     table)))
	(list level input (cdr kind))))))

(defun antlr-options-menu-filter (level _menu-items)
  "Return items for options submenu of level LEVEL."
  (let ((active (if buffer-read-only
		    nil
		  (consp (cdr-safe (cdr (antlr-option-kind level)))))))
    (mapcar (lambda (option)
	      (vector option
		      (list 'antlr-insert-option level option)
		      :active active))
	    (sort (mapcar #'car (elt antlr-options-alists (1- level)))
		  #'string-lessp))))


;;;===========================================================================
;;;  Insert option: determine section-kind
;;;===========================================================================

(defun antlr-option-kind (requested)
  "Return level and location for option to insert near point.
Call function `antlr-option-level' with argument REQUESTED.  If the
result is nil, return \(REQUESTED . error).  If the result has the
non-nil value LEVEL, return \(LEVEL . LOCATION) where LOCATION looks
like \(AREA . PLACE), see `antlr-option-location'."
  (save-excursion
    (save-restriction
      (let ((min0 (point-min))		; before `widen'!
	    (max0 (point-max))
	    (orig (point))
	    (level (antlr-option-level requested)) ; calls `widen'!
	    pos)
	(cond ((null level)
	       (setq level requested))
	      ((eq level 1)		; file options
	       (goto-char (point-min))
	       (setq pos (antlr-skip-file-prelude 'header-only)))
	      ((not (eq level 3))	; grammar or subrule options
	       (setq pos (point))
	       (antlr-c-forward-sws))
	      ((looking-at "^\\(private[ \t\n]\\|public[ \t\n]\\|protected[ \t\n]\\|fragment[ \t\n]\\)?[ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]*\\(!\\)?[ \t\n]*\\(\\[\\)?")
	       ;; rule options, with complete rule header
	       (goto-char (or (match-end 4) (match-end 3)))
	       (setq pos (antlr-skip-sexps (if (match-end 5) 1 0)))
	       (when (looking-at "returns[ \t\n]*\\[")
		 (goto-char (1- (match-end 0)))
		 (setq pos (antlr-skip-sexps 1)))))
	(cons level
	      (cond ((null pos) 'error)
		    ((looking-at "options[ \t\n]*{")
		     (goto-char (match-end 0))
		     (setq pos (ignore-errors (scan-lists (point) 1 1)))
		     (antlr-option-location orig min0 max0
					    (point)
					    (if pos (1- pos) (point-max))
					    t))
		    (t
		     (antlr-option-location orig min0 max0
					    pos (point)
					    nil))))))))

(defun antlr-option-level (requested)
  "Return level for option to insert near point.
Remove any restrictions from current buffer and return level for the
option to insert near point, i.e., 1, 2, 3, 4, or nil if no such option
can be inserted.  If REQUESTED is non-nil, it is the only possible value
to return except nil.  If REQUESTED is nil, return level for the nearest
option kind, i.e., the highest number possible.

If the result is 2, point is at the beginning of the class after the
class definition.  If the result is 3 or 4, point is at the beginning of
the rule/subrule after the init action.  Otherwise, the point position
is undefined."
  (widen)
  (if (eq requested 1)
      (and (car antlr-options-alists) 1)
    (let* ((orig (point))
           (outsidep (antlr-outside-rule-p))
           bor)
      (setq bor (point))		; beginning of rule
      (cond ((eq requested 2)		; grammar options required?
             (antlr-try-rule-or-grammar-option requested bor))
            ((and (car antlr-options-alists) ; file options available (v2)
                  (save-excursion		 ; in region of file options?
                    (goto-char (point-min))
                    (antlr-skip-file-prelude t) ; ws/comment after: OK
                    (< orig (point))))
             (and (null requested) 1))
            (outsidep			; outside -> grammar option
             (when (memq requested '(nil 2))
               (antlr-try-rule-or-grammar-option 2 bor)))
            ((looking-at antlr-grammar-header-regexp) ; rule = class def?
             (goto-char (match-end 0))
             (and (null requested) 2))
            ((or (eq requested 3) (null (elt antlr-options-alists 3)))
             (antlr-try-rule-or-grammar-option requested bor))
            ((antlr-syntactic-grammar-depth orig bor t)
             4)
            (t
             (antlr-try-rule-or-grammar-option requested bor))))))

(defun antlr-try-rule-or-grammar-option (requested bor)
  "Try whether rule or grammar option can be applied.
Called by function `antlr-option-level' with arguments REQUESTED,
and BOR pointing to the beginning of the current rule."
  (or (and (memq requested '(nil 3)) (elt antlr-options-alists 2)
           (progn (goto-char bor) 3))
      (and (memq requested '(nil 2)) (cadr antlr-options-alists)
           (let (boc)		; beginning of class
             (goto-char (point-min))
             (while (and (<= (point) bor)
                         (antlr-re-search-forward antlr-grammar-header-regexp nil))
               (if (<= (match-beginning 0) bor)
                   (setq boc (match-end 0))))
             (when boc
               (goto-char boc)
               2)))))

(defun antlr-option-location (orig min-vis max-vis min-area max-area withp)
  ;; checkdoc-order: nil
  "Return location for the options area.
ORIG is the original position of `point', MIN-VIS is `point-min' and
MAX-VIS is `point-max'.  If WITHP is non-nil, there exists an option
specification and it starts after the brace at MIN-AREA and stops at
MAX-AREA.  If WITHP is nil, there is no area and the region where it
could be inserted starts at MIN-AREA and stops at MAX-AREA.

The result has the form (AREA . PLACE).  AREA is (MIN-AREA . MAX-AREA)
if WITHP is non-nil, and nil otherwise.  PLACE is nil if the area is
invisible, (ORIG) if ORIG is inside the area, (MIN-AREA . beginning) for
a visible start position and (MAX-AREA . end) for a visible end position
where the beginning is preferred if WITHP is nil and the end if WITHP is
non-nil."
  (cons (and withp (cons min-area max-area))
	(cond ((and (<= min-area orig) (<= orig max-area)
		    (save-excursion
		      (goto-char orig)
		      (not (memq (antlr-syntactic-context)
				 '(comment block-comment)))))
	       ;; point in options area and not in comment
	       (list orig))
	      ((and (null withp) (<= min-vis min-area) (<= min-area max-vis))
	       ;; use start of options area (only if not `withp')
	       (cons min-area 'beginning))
	      ((and (<= min-vis max-area) (<= max-area max-vis))
	       ;; use end of options area
	       (cons max-area 'end))
	      ((and withp (<= min-vis min-area) (<= min-area max-vis))
	       ;; use start of options area (only if `withp')
	       (cons min-area 'beginning)))))

(defun antlr-syntactic-grammar-depth (pos beg &optional outside-action)
  "Return syntactic context depth at POS.
Move to POS and from there on to the beginning of the string or comment
if POS is inside such a construct.  Then, return the syntactic context
depth at point if the point position is smaller than BEG.
WARNING: this may alter `match-data'.
With optional argument OUTSIDE-ACTION, move to beginning of action."
  (goto-char pos)
  (let ((ppss (syntax-ppss)))
    (when (or (nth 3 ppss) (nth 4 ppss)) ; string or comment -> to beginning
      (goto-char (nth 8 ppss)))
    (when outside-action
      (let ((poss (nth 9 ppss))        ; TODO: syntax-ppss-open-positions
            open)
        (while (and poss (eq (char-after (car poss)) ?\())
          (setq open (pop poss)))
        (when open
          (goto-char (1+ open))
          (>= open beg))))))


;;;===========================================================================
;;;  Insert options: do the insertion
;;;===========================================================================

(defun antlr-insert-option-do (level option old area pos)
  ;; checkdoc-order: nil
  "Insert option into buffer at position POS.
Insert option of level LEVEL and name OPTION.  If OLD is non-nil, an
options area is already exists.  If OLD looks like \(BEG . END), the
option already exists.  Then, BEG is the start position of the option
value, the position of the `=' or nil, and END is the end position of
the option value or nil.

If the original point position was outside an options area, AREA is nil.
Otherwise, and if an option specification already exists, AREA is a cons
cell where the two values determine the area inside the braces."
  (let* ((spec (cdr (assoc option (elt antlr-options-alists (1- level)))))
	 (value (cdr spec)))
    (if (fboundp (car spec)) (funcall (car spec) 'before-input option))
    ;; set mark (unless point was inside options area before)
    (if (cond (area (eq antlr-options-push-mark t))
	      ((numberp antlr-options-push-mark)
	       (> (count-lines (min (point) pos) (max (point) pos))
		  antlr-options-push-mark))
	      (antlr-options-push-mark))
	(push-mark))
    ;; read option value -----------------------------------------------------
    (goto-char pos)
    (if (null value)
	;; no option specification found
	(if (y-or-n-p (format "Insert unknown %s option %s? "
			      (elt antlr-options-headings (1- level))
			      option))
	    (message "Insert value for %s option %s"
		     (elt antlr-options-headings (1- level))
		     option)
	  (error "Didn't insert unknown %s option %s"
		 (elt antlr-options-headings (1- level))
		 option))
      ;; option specification found
      (if (car value)
	  (let ((initial (and (consp old) (cdr old)
			      (buffer-substring (car old) (cdr old)))))
	    (setq value (apply (car value)
			       (and initial
				    (if (eq (aref initial 0) ?\")
					(read initial)
				      initial))
			       (cdr value))))
	(message "%s" (or (cadr value) ""))
	(setq value nil)))
    ;; insert value ----------------------------------------------------------
    (if (consp old)
	(antlr-insert-option-existing old value)
      (if (consp area)
	  ;; Move outside string/comment if point is inside option spec
	  (antlr-syntactic-grammar-depth (point) (car area)))
      (antlr-insert-option-space area old)
      (or old (antlr-insert-option-area level))
      (insert option " = ;")
      (backward-char)
      (if value (insert value)))
    ;; final -----------------------------------------------------------------
    (if (fboundp (car spec)) (funcall (car spec) 'after-insertion option))))


;;;===========================================================================
;;;  Insert options: the details (used by `antlr-insert-option-do')
;;;===========================================================================

(defun antlr-insert-option-existing (old value)
  ;; checkdoc-order: nil
  "Insert option value VALUE at point for existing option.
For OLD, see `antlr-insert-option-do'."
  ;; no = => insert =
  (unless (car old) (insert antlr-options-assign-string))
  ;; with user input => insert if necessary
  (when value
    (if (cdr old)		; with value
	(if (string-equal value (buffer-substring (car old) (cdr old)))
	    (goto-char (cdr old))
	  (delete-region (car old) (cdr old))
	  (insert value))
      (insert value)))
  (unless (looking-at "\\([^\n=;{}/'\"]\\|'\\([^\n'\\]\\|\\\\.\\)*'\\|\"\\([^\n\"\\]\\|\\\\.\\)*\"\\)*;")
    ;; stuff (no =, {, } or /) at point is not followed by ";"
    (insert ";")
    (backward-char)))

(defun antlr-insert-option-space (area old)
  "Find appropriate place to insert option, insert newlines/spaces.
For AREA and OLD, see `antlr-insert-option-do'."
  (let ((orig (point))
	(open t))
    (skip-chars-backward " \t")
    (unless (bolp)
      (let ((before (char-after (1- (point)))))
	(goto-char orig)
	(and old			; with existing options area
	     (consp area)		; if point inside existing area
	     (not (eq before ?\;))	; if not at beginning of option
					; => skip to end of option
	     (if (and (search-forward ";" (cdr area) t)
		      (let ((context (antlr-syntactic-context)))
			(or (null context) (numberp context))))
		 (setq orig (point))
	       (goto-char orig)))
	(skip-chars-forward " \t")

	(if (looking-at "$\\|//")
	    ;; just comment after point => skip (+ lines with same col comment)
	    (let ((same (if (> (match-end 0) (match-beginning 0))
			    (current-column))))
	      (beginning-of-line 2)
	      (or (bolp) (insert "\n"))
	      (when (and same (null area)) ; or (consp area)?
		(while (and (looking-at "[ \t]*\\(//\\)")
			    (goto-char (match-beginning 1))
			    (= (current-column) same))
		  (beginning-of-line 2)
		  (or (bolp) (insert "\n")))))
	  (goto-char orig)
	  (if (null old)
	      (progn (insert "\n") (antlr-indent-line))
	    (unless (eq (char-after (1- (point))) ?\ )
	      (insert " "))
	    (unless (eq (char-after (point)) ?\ )
	      (insert " ")
	      (backward-char))
	    (setq open nil)))))
    (when open
      (beginning-of-line 1)
      (insert "\n")
      (backward-char)
      (antlr-indent-line))))

(defun antlr-insert-option-area (level)
  "Insert new options area for options of level LEVEL.
Used by `antlr-insert-option-do'."
  (insert "options {\n\n}")
  (when (and antlr-options-auto-colon
	     (memq level '(3 4))
	     (save-excursion
	       (antlr-c-forward-sws)
	       (if (eq (char-after (point)) ?\{) (antlr-skip-sexps 1))
	       (not (eq (char-after (point)) ?\:))))
    (insert "\n:")
    (antlr-indent-line)
    (end-of-line 0))
  (backward-char 1)
  (antlr-indent-line)
  (beginning-of-line 0)
  (antlr-indent-line))


;;;===========================================================================
;;;  Insert options: in `antlr-options-alists'
;;;===========================================================================

(defun antlr-read-value ( initial-contents prompt
			  &optional as-string table table-x)
  "Read a string from the minibuffer, possibly with completion.
If INITIAL-CONTENTS is non-nil, insert it in the minibuffer initially.
PROMPT is a string to prompt with, normally it ends in a colon and a
space.  If AS-STRING is non-nil, return printed representation of the user
input, otherwise return the user input directly.

If TABLE or TABLE-X is non-nil, read with completion.  The completion
table is the resulting alist of TABLE-X concatenated with TABLE where
TABLE can also be a function evaluation to an alist.

Used inside `antlr-options-alists'."
  (let* ((completion-ignore-case t)	; dynamic
	 (table0 (and (or table table-x)
		      (append table-x
			      (if (functionp table) (funcall table) table))))
	 (input (if table0
		    (completing-read prompt table0 nil nil initial-contents)
		  (read-from-minibuffer prompt initial-contents))))
    (if as-string
        ;; if necessary, `use print-escape-newlines', `print-escape-nonascii'
        ;; if strings should be used in v3/v4, write our own (single quote)
        (format "%S" input)
      input)))

(defun antlr-read-boolean (initial-contents prompt &optional table)
  "Read a boolean value from the minibuffer, with completion.
If INITIAL-CONTENTS is non-nil, insert it in the minibuffer initially.
PROMPT is a string to prompt with, normally it ends in a question mark
and a space.  \"(true or false) \" is appended if TABLE is nil.

Without TABLE, use `y-or-n-p', otherwise read with completion
over \"true\", \"false\" and the keys in TABLE, see also
`antlr-read-value'.

Used inside `antlr-options-alists'."
  (if table
      (antlr-read-value initial-contents prompt
                        nil table '(("false") ("true")))
    (if (y-or-n-p prompt) "true" "false")))

(defun antlr-read-language (initial-contents prompt)
  "Read an action language from the minibuffer, with completion.
If INITIAL-CONTENTS is non-nil, insert it in the minibuffer initially.
PROMPT is a string to prompt with, normally it ends in a colon and a
space.

Used inside `antlr-options-alists'."
  (let ((table (apply #'nconc
                      (mapcar (lambda (l) (mapcar #'list (cdr l)))
                              antlr-language-list))))
    (antlr-read-value initial-contents prompt nil table)))

(defun antlr-language-option-extra (phase &rest _dummies)
  "Change language according to the new value of the \"language\" option.
Call `antlr-mode' if the new language would be different from the value
of `antlr-language', keeping the value of variable `font-lock-mode'.

Called in PHASE `after-insertion', see `antlr-options-alists'."
  (when (eq phase 'after-insertion)
    (let ((new-language (antlr-guess-language)))
      (or (null new-language)
	  (eq new-language antlr-language)
	  (let ((font-lock (and (boundp 'font-lock-mode) font-lock-mode)))
	    (if font-lock (font-lock-mode 0))
	    (funcall major-mode)                ; TODO: do differently?
	    (and font-lock (null font-lock-mode) (font-lock-mode 1)))))))

(defun antlr-c++-mode-extra (phase option &rest _dummies)
  ;; checkdoc-order: nil
  "Warn if C++ option OPTION is used with the wrong language.
Ask user \(\"y or n\"), if a C++ only option is going to be inserted but
`antlr-language' has not the value `antlr-cpp'.

Called in PHASE `before-input', see `antlr-options-alists'."
  (and (eq phase 'before-input)
       (not (eq antlr-language 'antlr-cpp))
       (not (y-or-n-p (format "Insert C++ %s option? " option)))
       (error "Didn't insert Cpp %s option with language %s"
	      option antlr-language-mode-name)))


;;;===========================================================================
;;;  Compute dependencies
;;;===========================================================================
;; This whole section is ANTLR-v2 specific.  It is not used in v3 and v4.

(defun antlr-file-dependencies ()
  "Return dependencies for grammar in current buffer.
This function is for ANTLR v2 grammars only.

The result looks like \(FILE \(CLASSES . SUPERS) VOCABS .  LANGUAGE)
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
  (let (classes export-vocabs import-vocabs superclasses default-vocab)
    (goto-char (point-min))
    (while (antlr-re-search-forward antlr-grammar-header-regexp nil)
      ;; parse class definition --------------------------------------------
      (let* ((class (match-string-no-properties 2))
             (sclass (match-string-no-properties 4))
             ;; export vocab defaults to class name (first grammar in file)
             ;; or to the export vocab of the first grammar in file:
             (evocab (or default-vocab class))
             (ivocab nil))
        (goto-char (match-end 0))
        (antlr-c-forward-sws)
        (while (looking-at "options\\_>\\|\\(tokens\\)\\_>")
          (if (match-beginning 1)
              (antlr-skip-sexps 2)
            (goto-char (match-end 0))
            (antlr-c-forward-sws)
            ;; parse grammar option sections -------------------------------
            (when (eq (char-after (point)) ?\{)
              (let* ((beg (1+ (point)))
                     (end (1- (antlr-skip-sexps 1)))
                     (cont (point)))
		(goto-char beg)
		(if (re-search-forward "\\<exportVocab[ \t]*=[ \t]*\\([A-Za-z\300-\326\330-\337]\\(?:\\sw\\|\\s_\\)*\\)" end t)
		    (setq evocab (match-string-no-properties 1)))
		(goto-char beg)
		(if (re-search-forward "\\<importVocab[ \t]*=[ \t]*\\([A-Za-z\300-\326\330-\337]\\(?:\\sw\\|\\s_\\)*\\)" end t)
		    (setq ivocab (match-string-no-properties 1)))
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
        (or (member evocab export-vocabs) (push evocab export-vocabs))
        (or (null ivocab)
            (member ivocab import-vocabs) (push ivocab import-vocabs))))
    (if classes
	(cl-list* (file-name-nondirectory buffer-file-name)
                  (cons (nreverse classes) (nreverse superclasses))
                  (cons (nreverse export-vocabs) (nreverse import-vocabs))
                  antlr-action-mode))))

(defun antlr-directory-dependencies (dirname)
  "Return dependencies for all grammar files in directory DIRNAME.
This function is for ANTLR v2 grammars only.

The result looks like \((CLASS-SPEC ...) . \(FILE-DEP ...))
  where CLASS-SPEC = (CLASS (FILE . EVOCAB) ...).

FILE-DEP are the dependencies for each grammar file in DIRNAME, see
`antlr-file-dependencies'.  For each grammar class CLASS, FILE is a
grammar file in which CLASS is defined and EVOCAB is the name of the
export vocabulary specified in that file."
  (let ((grammar (directory-files dirname t "\\.g\\'")))
    (when grammar
      (let ((antlr-imenu-name nil)		; dynamic-let: no imenu
	    (expanded-regexp
             (concat (format (regexp-quote
                              (cadr antlr-special-file-formats))
                             ".+")
                     "\\'"))
	    classes dependencies)
        (with-temp-buffer
          (dolist (file grammar)
            (when (and (file-regular-p file)
                       (null (string-match expanded-regexp file)))
              (insert-file-contents file t nil nil t)
              (normal-mode t)           ; necessary for major-mode, syntax
					; table and `antlr-language'
              (when (and (derived-mode-p 'antlr-mode)
                         (eq antlr-tool-version 'antlr-v2))
                (let* ((file-deps (antlr-file-dependencies))
                       (file (car file-deps)))
                  (when file-deps
                    (dolist (class-def (cl-caadr file-deps))
                      (let ((file-evocab (cons file (cdr class-def)))
                            (class-spec (assoc (car class-def) classes)))
                        (if class-spec
                            (nconc (cdr class-spec) (list file-evocab))
                          (push (list (car class-def) file-evocab)
                                classes))))
                    (push file-deps dependencies)))))))
	(cons (nreverse classes) (nreverse dependencies))))))

(defun antlr-superclasses-glibs (supers classes)
  "Compute the grammar lib option for the super grammars SUPERS.
This function is for ANTLR v2 grammars only.

Look in CLASSES for the right grammar lib files for SUPERS.  SUPERS is
part SUPER in the result of `antlr-file-dependencies'.  CLASSES is the
part \(CLASS-SPEC ...) in the result of `antlr-directory-dependencies'.

The result looks like \(OPTION WITH-UNKNOWN GLIB ...).  OPTION is the
complete \"-glib\" option.  WITH-UNKNOWN is t if there is none or more
than one grammar file for at least one super grammar.

Each GLIB looks like \(GRAMMAR-FILE . EVOCAB).  GRAMMAR-FILE is a file
in which a super-grammar is defined.  EVOCAB is the value of the export
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
    (cons (if glibs (concat " -glib " (mapconcat #'car glibs ";")) "")
	  (cons unknown glibs))))


;;;===========================================================================
;;;  Compilation: run ANTLR tool
;;;===========================================================================

(defvar antlr-grammar-file nil
  "Grammar file processed in *compilation* buffer.
Set by command `antlr-run-tool’.")

(defun antlr-grammar-file ()
  "Return the current grammar file.
This function could be useful in `antlr-compilation-error-regexp-alist'
for certain tools."
  antlr-grammar-file)

(defun antlr-run-tool (command file &optional saved)
  "Run Antlr took COMMAND on grammar FILE.
When called interactively, COMMAND is read from the minibuffer and
defaults to `antlr-tool-command', with a computed \"-glib\" option for
ANTLR v2 grammars if necessary.
See also variable `antlr-run-tool-on-buffer-file'.

Save all buffers first unless optional value SAVED is non-nil.  When
called interactively, the buffers are always saved, see also variable
`antlr-ask-about-save'."
  (interactive (antlr-run-tool-interactive))
  (or saved (save-some-buffers (not antlr-ask-about-save)))
  (let ((default-directory (file-name-directory file))
        (error-regexp-alist antlr-compilation-error-regexp-alist)
        (process-environment
         (if antlr-tool-path
             (let ((path (mapconcat #'substitute-env-vars
                                    antlr-tool-path path-separator)))
               (cons (concat "PATH=" path path-separator
                             (getenv "PATH"))
                     (cons (concat "LD_LIBRARY_PATH=" path path-separator
                                   (getenv "LD_LIBRARY_PATH"))
                           process-environment)))
           process-environment))) ;#dynamic
    ;; the MODE argument of `compilation-start' is quite a hack...
    (with-current-buffer
        (compilation-start command antlr-compilation-mode
                           (lambda (_mode-name) "*Antlr-Run*"))
      (when error-regexp-alist
        (setq-local compilation-error-regexp-alist error-regexp-alist))
      (setq-local antlr-grammar-file file))))

(defun antlr-run-tool-interactive ()
  ;; code in `interactive' is not compiled
  "Interactive specification for `antlr-run-tool'.
Use prefix argument ARG to return \(COMMAND FILE SAVED)."
  (let* ((supers (and (eq antlr-tool-version 'antlr-v2)
                      (cl-cdadr (save-excursion
                                  (save-restriction
                                    (widen)
                                    (antlr-file-dependencies))))))
         (prompt (if (functionp antlr-tool-command)
                     (funcall antlr-tool-command buffer-file-name supers)
                   (if (null supers)
                       antlr-tool-command
                     (save-some-buffers (not antlr-ask-about-save) nil)
                     (concat antlr-tool-command
                             (car (antlr-superclasses-glibs
                                   supers
                                   (car (antlr-directory-dependencies
                                         default-directory)))))))))
    (list (if antlr-run-tool-on-buffer-file
              (concat (read-shell-command "Run Antlr with: " prompt)
                      " " (file-name-nondirectory buffer-file-name))
            (read-shell-command "Run Antlr with: "
                                (concat prompt " "
                                        (file-name-nondirectory buffer-file-name))))
          buffer-file-name
          supers)))


;;;===========================================================================
;;;  Makefile creation
;;;===========================================================================
;; This whole section is ANTLR-v2 specific.

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
This function is for ANTLR v2 grammars only.

IN-MAKEFILE is non-nil, if the current buffer is the Makefile.  See
command `antlr-show-makefile-rules' for detail."
  (let* ((dirname default-directory)
	 (deps0 (antlr-directory-dependencies dirname))
	 (classes (car deps0))		; CLASS -> (FILE . EVOCAB) ...
	 (deps (cdr deps0))		; FILE -> (c . s) (ev . iv) . LANGUAGE
	 (with-error nil)
	 (gen-sep (or (cl-caddr (cadr antlr-makefile-specification)) " "))
	 (n (and (cdr deps) (cadr antlr-makefile-specification) 0)))
    (or in-makefile (set-buffer standard-output))
    (dolist (dep deps)
      (let ((supers (cl-cdadr dep))
	    (lang (cdr (assoc (cl-cdddr dep) antlr-file-formats-alist))))
	(if n (cl-incf n))
	(antlr-makefile-insert-variable n "" " =")
	(if supers
	    (insert " "
		    (format (cadr antlr-special-file-formats)
			    (file-name-sans-extension (car dep)))))
	(dolist (class-def (cl-caadr dep))
	  (let ((sep gen-sep))
	    (dolist (class-file (cadr lang))
	      (insert sep (format class-file (car class-def)))
	      (setq sep " "))))
	(dolist (evocab (cl-caaddr dep))
	  (let ((sep gen-sep))
	    (dolist (vocab-file (cons (car antlr-special-file-formats)
				      (car lang)))
	      (insert sep (format vocab-file evocab))
	      (setq sep " "))))
	(antlr-makefile-insert-variable n "\n$(" ")")
	(insert ": " (car dep))
	(dolist (ivocab (cl-cdaddr dep))
	  (insert " " (format (car antlr-special-file-formats) ivocab)))
	(let ((glibs (antlr-superclasses-glibs supers classes)))
	  (if (cadr glibs) (setq with-error t))
	  (dolist (super (cddr glibs))
	    (insert " " (car super))
	    (if (cdr super)
		(insert " " (format (car antlr-special-file-formats)
				    (cdr super)))))
	  (insert "\n\t"
		  (cl-caddr antlr-makefile-specification)
		  (car glibs)
		  " $<\n"
		  (car antlr-makefile-specification)))))
    (if n
	(let ((i 0))
	  (antlr-makefile-insert-variable nil "" " =")
	  (while (<= (cl-incf i) n)
	    (antlr-makefile-insert-variable i " $(" ")"))
	  (insert "\n" (car antlr-makefile-specification))))
    (if (bobp)
        (setq with-error t)
      (if (string-equal (car antlr-makefile-specification) "\n")
          (delete-char -1)))
    (when with-error
      (goto-char (point-min))
      (insert antlr-help-unknown-file-text))
    (unless in-makefile
      (copy-region-as-kill (point-min) (point-max))
      (goto-char (point-min))
      (insert (format (substitute-command-keys antlr-help-rules-intro)
                      dirname)))))

;;;###autoload
(defun antlr-show-makefile-rules ()
  "Show Makefile rules for all grammar files in the current directory.
This command is for ANTLR v2 grammars only.

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
  (unless (eq antlr-tool-version 'antlr-v2)
    (user-error (substitute-command-keys
                 "Run \\[antlr-run-tool] with option \"-depend\" instead")))
  (if (null (derived-mode-p 'makefile-mode))
      (with-output-to-temp-buffer (help-buffer)
        (save-excursion (antlr-insert-makefile-rules)))
    (push-mark)
    (antlr-insert-makefile-rules t)))


;;;===========================================================================
;;;  Indentation
;;;===========================================================================

(defun antlr-indent-line ()
  "Indent the current line as ANTLR grammar code.
The default indentation of grammar lines are calculated by
`c-basic-offset', multiplied by:
 - the level of the paren/brace/bracket depth,
 - plus 0/2/1, depending on the position inside the rule: header, body,
   exception part,  customized by `antlr-base-offset-alist',
 - minus 1 if `antlr-indent-item-regexp' matches the beginning of the
   line starting from the first non-whitespace.

Lines inside block comments are indented by `c-indent-line' according to
`antlr-indent-comment'.

Lines in actions except top level actions in a header part or an option
area are indented by `c-indent-line'.

Lines in header actions are indented at column 0 if `antlr-language'
equals to a key in `antlr-indent-at-bol-alist' and the line starting at
the first non-whitespace is matched by the corresponding value.

For the initialization of `c-basic-offset', see `antlr-indent-style' and,
to a lesser extent, `antlr-tab-offset-alist'."
  ;; TODO: this function needs to be rewritten
  (save-restriction
    (let ((orig (point))
	  (min0 (point-min))
	  bol boi indent syntax cc-syntax boa pdepth)
      (widen)
      (beginning-of-line)
      (setq bol (point))
      (if (< bol min0)
	  (error "Beginning of current line not visible"))
      (skip-chars-forward " \t")
      (setq boi (point))
      ;; check syntax at beginning of indentation ----------------------------
      (let* ((ppss (syntax-ppss))
             (context (antlr-syntactic-context ppss))
             (open (nth 9 ppss)))       ; TODO Emacs: syntax-ppss-open-positions
        (setq syntax (or context 0))
        (setq pdepth (car ppss))
        ;; TODO: should boa = first non-?\( in (nth 9 ppss) ?
        (when (numberp context)          ; boa = beginning of action
          (setq boa (nth (- (length open) context) open))))
      (cond ((symbolp syntax)
             (setq indent nil))	; block-comments, strings, (comments) -> cc engine
            ((progn
               (antlr-next-rule -1 t) ; to start of rule
               (= (point) boi))
             (setq indent 0))         ; rule start always at 0
            ;; TODO: use antlr-skip-to-colon-or-semi

            ((if (let ((r (antlr-search-forward antlr-rule-body-start-op)))
                   (while (and r (eq (char-after) ?:)) ; skip double-colon
                     (forward-char)
                     (setq r (antlr-search-forward antlr-rule-body-start-op)))
                   r)
                 (< boi (1- (point)))
               t)
             (setq indent
                   (or (cdr (assq :header antlr-base-offset-alist)) 0)))
            ((eq (char-after boi) ?:)
             (setq indent
                   (or (cdr (assq :colon antlr-base-offset-alist))
                       (cdr (assq :body antlr-base-offset-alist))
                       2)))
            ((if (antlr-search-forward ";" antlr-skip-line-regexp)
                 (< boi (point))
               t)
             (setq indent
                   (or (cdr (assq :body antlr-base-offset-alist)) 2)))
            (t
             (forward-char)
             (antlr-skip-rule-postlude nil)
             (setq indent
                   (if (> (point) boi)
                       (or (cdr (assq :exception antlr-base-offset-alist)) 1)
                     0)))) ; in exception part?
      ;; check whether to use indentation engine of cc-mode ------------------
      (goto-char boi)
      (when (and indent (> syntax 0))
	(cond ((> syntax 1)		; block in action => use cc-mode (or nothing)
	       (setq indent nil)
               (when antlr-indent-action-line
                 (setq syntax 'non-cc)))
	      ((and (= indent 0)        ; TODO: recheck
		    (assq antlr-action-mode antlr-indent-at-bol-alist)
		    (looking-at (cdr (assq antlr-action-mode
					   antlr-indent-at-bol-alist))))
	       (setq syntax 'bol))
              ((memq (char-after) '(?\} ?\]))) ; close the block -> usual grammar indent
              (antlr-indent-action-line
               ;; TODO: parameters, and mode-specific - should we handle options{} and tokens{} extra?
               (setq indent nil syntax 'non-cc))
	      ((setq cc-syntax (c-guess-basic-syntax))
	       (let ((cc cc-syntax) symbol)
		 (while (setq symbol (pop cc))
		   (when (cdr symbol)
		     (or (memq (car symbol)
			       antlr-disabling-cc-syntactic-symbols)
			 (setq indent nil))
		     (setq cc nil)))))))
      ;; compute the corresponding indentation and indent --------------------
      (if (null indent)
	  ;; Use the indentation engine of cc-mode
	  (progn
	    (goto-char orig)
            (cond ((eq syntax 'non-cc)
                   ;; TODO: we might check whether the current line is the
                   ;; first code in the action -> then we simply indent
                   ;; according to indent-level (then we do not have to check
                   ;; whether the LANGUAGE indents at col 0 (and use C indent)
                   (funcall antlr-indent-action-line boa))
                  ((or (numberp syntax)
                       (if (eq syntax 'string) nil (eq antlr-indent-comment t)))
                   (c-indent-line cc-syntax))))
	;; do it ourselves
	(goto-char boi)
	(unless (symbolp syntax)		; direct indentation
	  (cl-incf indent pdepth)
	  (and (> indent 0) (looking-at antlr-indent-item-regexp)
               (cl-decf indent))
	  (setq indent (* indent c-basic-offset)))
	;; the usual major-mode indent stuff ---------------------------------
        ;; TODO: use `indent-line-to' instead?
	(setq orig (- (point-max) orig))
	(unless (= (current-column) indent)
	  (delete-region bol boi)
	  (beginning-of-line)
	  (indent-to indent))
	;; If initial point was within line's indentation,
	;; position after the indentation.  Else stay at same point in text.
	(if (> (- (point-max) orig) (point))
	    (goto-char (- (point-max) orig)))))))

(defun antlr-indent-command (&optional arg)
  ;; TODO: drop this command if there is a extra function for `indent-region'
  "Indent the current line or insert tabs/spaces.
With optional prefix argument ARG, insert ARG tabs or spaces according
to `indent-tabs-mode'.
Otherwise, indent the current line with `antlr-indent-line'."
  (interactive "*P")
  (if arg
      (insert-tab arg)
    (let ((antlr-indent-comment (and antlr-indent-comment t))) ; dynamic
      (antlr-indent-line))))

(defun antlr-electric-character (&optional arg)
  "Insert the character you type and indent the current line.
Insert the character like `self-insert-command' and indent the current
line as `antlr-indent-command' does.  Do not indent the line if

 * this command is called with a prefix argument ARG,
 * there are characters except whitespaces between point and the
   beginning of the line, or
 * point is not inside a normal grammar code, { and } are also OK in
   actions.

This command is useful for a character which has some special meaning in
ANTLR's syntax and influences the auto indentation, see
`antlr-indent-item-regexp'."
  (interactive "*P")
  (if (or arg
	  (save-excursion (skip-chars-backward " \t") (not (bolp)))
          (let ((context (antlr-syntactic-context)))
            (and context
                 (not (and (numberp context)
                           (memq last-command-event '(?\{ ?\})))))))
      (self-insert-command (prefix-numeric-value arg))
    (self-insert-command (prefix-numeric-value arg))
    (antlr-indent-line)))

(defun antlr-insert-keyword-rule (&optional keyword)
  "Insert token rule for an case-insensitive keyword KEYWORD at point."
  (interactive "sKeyword: ")
  ;; Should be probably made a bit customizable...
  (insert (upcase keyword) " : "
          (mapconcat (lambda (c)
                       (let ((d (downcase c)) (u (upcase c)))
                         (cond ((eq d u) (string ?\' c ?\'))
                               ((eq antlr-tool-version 'antlr-v4)
                                (string ?\[ d u ?\]))
                               (t (string ?\( ?\' d ?\' ?| ?\' u ?\' ?\))))))
                     keyword "")
          " ;\n"))


;;;===========================================================================
;;;  Mode entry
;;;===========================================================================

(defun antlr-guess-language ()
  "Find language in `antlr-language-list' for language option.
If not found, use the default, which is the first element."
  ;; TODO: think about save-restriction & widen
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (cdr antlr-language-limit-n-regexp)
                             (+ (point)
                                (car antlr-language-limit-n-regexp))
                             t)
      (car (cl-find (match-string-no-properties 1) antlr-language-list
                    :key #'cdr :test #'member)))))

(defun antlr-guess-tool-version ()
  "Guess whether current grammar is for ANTLR v2 or v3.
By default, we assume v3.  Only if we find a keyword `class' or
`header' at the beginning of a line before any `grammar' keyword,
we assume v2."
  (save-excursion
    (goto-char (point-min))
    (if (and (antlr-re-search-forward "^\\(lexer[ \t]+grammar\\|parser[ \t]+grammar\\|tree[ \t]+grammar\\|grammar\\|\\(class\\|header\\)\\)\\_>" (+ (point) (car antlr-language-limit-n-regexp)))
             (match-beginning 2))      ; class or header
        'antlr-v2
      'antlr-v3)))

(defun antlr-skip-import-statement ()
  "Skip whitespaces, comments and special declarations after the header.
See  `antlr-skip-line-regexp', which skips the `scope' declaration in
ANTLR v3, and the `import' declaration in ANTLR v4."
  (if (not (and antlr-skip-line-regexp (looking-at antlr-skip-line-regexp)))
      (antlr-skip-sexps 1)
    (goto-char (match-end 0))
    (prog1 (point)
      (antlr-c-forward-sws))))

(defun antlr-set-local-variables (selector-symbol selector variables)
  ;; checkdoc-order: nil
  "Set SELECTOR dependent local variables for VARIABLES.
Also set SELECTOR-SYMBOL to SELECTOR.
See `antlr-tool-version-variables' and `antlr-language-variables'."
  (unless (symbolp selector)
    (error "Illegal selector %s" selector))
  (let ((required t) (prefix (symbol-name selector)))
    (dolist (var variables)
      (if (eq var '&optional)
          (setq required nil)
        (let ((name (symbol-name var)))
          (unless (and (string-match "\\`antlr-" name) (boundp var))
            (error "Illegal element %s" var))
          (let ((valsym (intern (concat prefix (substring name 5)))))
            (when (or (boundp valsym) required)
              (set (make-local-variable var) (symbol-value valsym)))))))
    (set (make-local-variable selector-symbol) selector)
    nil))

;; I would have guessed that there is a possibility to run code at the end of
;; initializing a major mode which is definitely run at the end (with or
;; without local variables enabled).  That is not the case... (only hard-coded
;; for font-lock).  Hence, we run some code twice with local variables...

(defun antlr-hack-local-variables-hook ()
  "Late setup for buffers with a local variables spec.
This function is used in `hack-local-variables-hook'."
  (and (derived-mode-p 'antlr-mode)
       (or (assq 'antlr-tool-version file-local-variables-alist)
           (assq 'antlr-language file-local-variables-alist))
       (antlr-set-tool-version-and-mode-line)))

(add-hook 'hack-local-variables-hook #'antlr-hack-local-variables-hook)

(defun antlr-set-tool-version-and-mode-line ()
  ;; FIXME: This does a *lot* more setup than is normal for
  ;; `hack-local-variables-hook'.
  "Late setup for `antlr-mode' and sub modes."
  ;; tool and language version and dependent variables -----------------------
  (let ((guess (if (local-variable-p 'antlr-tool-version)
                   ;; backward-compatibility:
                   (if (numberp antlr-tool-version) 'antlr-v2 antlr-tool-version)
                 (antlr-guess-tool-version))))
    (when (with-demoted-errors "File mode error for `antlr-tool-version': %s"
            (antlr-set-local-variables 'antlr-tool-version guess
                                       antlr-tool-version-variables))
      (antlr-set-local-variables 'antlr-tool-version 'antlr-v3
                                 antlr-tool-version-variables)))
  (let ((guess (if (local-variable-p 'antlr-language)
                   (or (cdr (assq antlr-language   ; backward compatibility
                                  '((java-mode . antlr-java) (c++-mode . antlr-cpp))))
                       antlr-language)
                 (antlr-guess-language))))
    (when (with-demoted-errors "File mode error for `antlr-language': %s"
            (antlr-set-local-variables 'antlr-language
                                       (or guess (caar antlr-language-list))
                                       antlr-language-variables))
      (antlr-set-local-variables 'antlr-language 'antlr-java
                                 antlr-language-variables)))
  ;; language-dependent initializations --------------------------------------
  (c-init-language-vars-for antlr-init-cc-mode)
  (c-basic-common-init antlr-init-cc-mode ; sets `indent-line-function' etc
                       (or antlr-indent-style "gnu"))
  (funcall antlr-init-submode)
  (set (make-local-variable 'indent-line-function) #'antlr-indent-line)
  (kill-local-variable 'indent-region-function) ;FIXME: Needed?
  ;; syntax-propertize -------------------------------------------------------
  (when antlr-syntax-propertize
    (setq-local syntax-propertize-function (car antlr-syntax-propertize))
    (unless (eq (cadr antlr-syntax-propertize) t)
      (if (functionp (cadr antlr-syntax-propertize))
          (add-hook 'syntax-propertize-extend-region-functions
                    (cadr antlr-syntax-propertize) 'append 'local)
        (setq-local syntax-propertize-extend-region-functions
                    (cadr antlr-syntax-propertize))))
    (when (nth 2 antlr-syntax-propertize)
      (setq-local c-multiline-string-start-char
                  (if c-multiline-string-start-char t
                    (nth 2 antlr-syntax-propertize))))
    (when antlr-do-syntax-propertize    ; at least for imenu in Emacs-24.5
      (syntax-propertize (point-max))))
  ;; Before I had moved `imenu-add-to-menubar' from function `antlr-mode', it
  ;; had already scanned the buffer before I have set `antlr-tool-version' here
  ;; (variables which depend on it).  Thus, I move the imenu function to this
  ;; function, too... (do we need to explicitly call `imenu-update-menubar' to
  ;; be on the safe side?)
  (and antlr-imenu-name			; there should be a global variable...
       (fboundp 'imenu-add-to-menubar)
       (imenu-add-to-menubar
	(if (stringp antlr-imenu-name) antlr-imenu-name "Index")))
  ;; FIXME: No need to set this so late.
  (setq mode-name '("" antlr-tool-mode-name "." antlr-language-mode-name)))


(defvar antlr-delayed-mode-hook '(antlr-set-tool-version-and-mode-line)
  "Hook run after entering Antlr mode or a derived mode.")

;;;###autoload
(define-derived-mode antlr-mode prog-mode
  "Antlr"
  "Major mode for editing ANTLR grammar files."
  :abbrev-table antlr-mode-abbrev-table
  (c-initialize-cc-mode)		; cc-mode is required
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'outline-regexp) "[^#\n\^M]")
  (set (make-local-variable 'outline-level) #'c-outline-level) ;TODO: define own
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip "/\\*+ *\\|//+ *")
  ;; various -----------------------------------------------------------------
  ;; the following vars are auto buffer-local:
  (setq font-lock-defaults antlr-font-lock-defaults)
  (setq imenu-create-index-function #'antlr-imenu-create-index-function)
  (setq imenu-generic-expression t)     ; fool stupid test
  ;; FIXME: How does this hook differ from `antlr-mode-hook'?
  (run-mode-hooks 'antlr-delayed-mode-hook))

;; A smarter version of `group-buffers-menu-by-mode-then-alphabetically' (in
;; XEmacs) could use the following property.  The header of the submenu would
;; be "Antlr" instead of "Antlr.C++" or (not and!) "Antlr.Java".
(put 'antlr-mode 'mode-name "Antlr")

;;;###autoload
(define-derived-mode antlr-v4-mode antlr-mode
  "Antlr" ; use this in the menubar, see below for the mode line
  "Major mode for editing ANTLR v4 grammar files."
  :syntax-table nil
  :abbrev-table nil
  (set (make-local-variable 'antlr-tool-version) 'antlr-v4))

;;;###autoload
(defun antlr-set-tabs ()
  "Use ANTLR's convention for TABs according to `antlr-tab-offset-alist'.
Used in `antlr-mode' for cc-mode-based languages.
It is probably better to automatically deduce the TAB setting."
  (if buffer-file-name
      (let ((alist antlr-tab-offset-alist) elem)
	(while alist
	  (setq elem (pop alist))
	  (and (or (null (car elem)) (eq (car elem) major-mode))
	       (or (null (cadr elem))
		   (string-match (cadr elem) buffer-file-name))
	       (setq tab-width (cl-caddr elem)
		     indent-tabs-mode (cl-cadddr elem)
		     alist nil))))))


;;;===========================================================================
;;;  CC-mode languages
;;;===========================================================================

(defvar antlr-c-language-mode-name "C"
  "Value for `antlr-language-mode-name' when using language `antlr-c'.")
(defvar antlr-cpp-language-mode-name "Cpp"
  "Value for `antlr-language-mode-name' when using language `antlr-cpp'.")
(defvar antlr-objc-language-mode-name "ObjC"
  "Value for `antlr-language-mode-name' when using language `antlr-objc'.")

(defvar antlr-c-action-mode 'c-mode
  "Value for `antlr-action-mode' when using language `antlr-c'.")
(defvar antlr-cpp-action-mode 'c++-mode
  "Value for `antlr-action-mode' when using language `antlr-cpp'.")
(defvar antlr-objc-action-mode 'objc-mode
  "Value for `antlr-action-mode' when using language `antlr-objc'.")

(defvar antlr-c-init-cc-mode 'c-mode
  "Value for `antlr-init-cc-mode' when using language `antlr-c'.")
(defvar antlr-cpp-init-cc-mode 'c++-mode
  "Value for `antlr-init-cc-mode' when using language `antlr-cpp'.")
(defvar antlr-obj-init-cc-mode 'objc-mode
  "Value for `antlr-init-cc-mode' when using language `antlr-objc'.")

(defvar antlr-c-action-font-lock-keywords
  '(antlr-no-action-keywords
    c-font-lock-keywords-1 c-font-lock-keywords-2
    c-font-lock-keywords-3)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-c'.")
(defvar antlr-cpp-action-font-lock-keywords
  '(antlr-no-action-keywords
    c++-font-lock-keywords-1 c++-font-lock-keywords-2
    c++-font-lock-keywords-3)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-cpp'.")
(defvar antlr-objc-action-font-lock-keywords
  '(antlr-no-action-keywords
    objc-font-lock-keywords-1 objc-font-lock-keywords-2
    objc-font-lock-keywords-3)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-objc'.")


;;;===========================================================================
;;;  JavaScript
;;;===========================================================================

(declare-function js-indent-line "js")
;; TODO: better support for js2-mode?

(defvar antlr-js-language-mode-name "Js"
  "Value for `antlr-language-mode-name' when using language `antlr-js'.")

(defvar antlr-js-action-mode 'js-mode
  "Value for `antlr-action-mode' when using language `antlr-js'.")

(defvar antlr-js-init-submode #'antlr-init-js
  "Value for `antlr-init-submode' when using language `antlr-js'.")

(defvar antlr-js-action-font-lock-keywords
  '(antlr-no-action-keywords
    ;; do not use `js--font-lock-keywords-3' !
    js--font-lock-keywords-1 js--font-lock-keywords-2)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-js'.")

(defvar antlr-js-indent-action-line #'antlr-js-indent-action-line
  "Value for `antlr-indent-action-line' when using language `antlr-js'.")

(defun antlr-init-js ()
  "Initialize action language `antlr-js'."
  (require 'js))

(defun antlr-js-indent-action-line (_boa)
  "Indent the current line in an JavaScript action."
  (js-indent-line))


;;;===========================================================================
;;;  Delphi (opascal)
;;;===========================================================================

(declare-function opascal-indent-line "opascal")
(defvar opascal-compound-block-indent)
(defvar opascal-indent-level)
(defvar opascal-case-label-indent)

(defvar antlr-delphi-language-mode-name "Delphi"
  "Value for `antlr-language-mode-name' when using language `antlr-delphi'.")

(defvar antlr-delphi-action-mode 'opascal-mode
  "Value for `antlr-action-mode' when using language `antlr-delphi'.")

(defvar antlr-delphi-init-submode #'antlr-init-delphi
  "Value for `antlr-init-submode' when using language `antlr-delphi'.")

(defvar antlr-delphi-action-font-lock-keywords
  '(antlr-no-action-keywords
    opascal-font-lock-keywords)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-delphi'.")

(defvar antlr-delphi-indent-action-line #'antlr-delphi-indent-action-line
  "Value for `antlr-indent-action-line' when using language `antlr-delphi'.")

(defun antlr-init-delphi ()
  "Initialize action language `antlr-delphi'."
  (require 'opascal)
  (when (integerp c-basic-offset)
    (when (equal opascal-compound-block-indent opascal-indent-level)
      (setq-local opascal-compound-block-indent c-basic-offset))
    (when (equal opascal-case-label-indent opascal-indent-level)
      (setq-local opascal-case-label-indent c-basic-offset))
    (setq-local opascal-indent-level c-basic-offset)))

(defun antlr-delphi-indent-action-line (boa)
  "Indent the current line in a Delphi (opascal) action.
Argument BOA is the start position of the action."
  (narrow-to-region (1+ boa) (line-end-position))
  ;; make Pascal mode only checks the code fragment after the opening brace,
  ;; otherwise its indentation gets confused as {...} are block comments
  (opascal-indent-line)
  ;; or use low-level `opascal-corrected-indentation' - but that does not have
  ;; a docstring, i.e. not really official ?
  (widen)
  (unless (memq (char-after (line-beginning-position)) '(?\ ?\t))
    ;; no indentation -> considered top-level -> indentation can also be
    ;; performed by c-mode
    (c-indent-line)))


;;;===========================================================================
;;;  Ruby
;;;===========================================================================

(declare-function ruby-indent-line "ruby-mode")
(defvar ruby-indent-level)
(defvar ruby-use-smie)

(defvar antlr-ruby-language-mode-name "Ruby"
  "Value for `antlr-language-mode-name' when using language `antlr-ruby'.")

(defvar antlr-ruby-action-mode 'ruby-mode
  "Value for `antlr-action-mode' when using language `antlr-ruby'.")

(defvar antlr-ruby-init-submode #'antlr-init-ruby
  "Value for `antlr-init-submode' when using language `antlr-ruby'.")

(defvar antlr-ruby-action-font-lock-keywords
  '(antlr-no-action-keywords
    ruby-font-lock-keywords)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-ruby'.")

(defvar antlr-ruby-indent-action-line #'antlr-ruby-indent-action-line
  "Value for `antlr-indent-action-line' when using language `antlr-ruby'.")

(defun antlr-init-ruby ()
  "Initialize action language `antlr-ruby'."
  (require 'ruby-mode)
  (setq-local ruby-indent-level c-basic-offset)
  ;; Disable smie as long as we do not have a function for a part of
  ;; `ruby-mode-variables'
  (setq-local ruby-use-smie nil))

(defun antlr-ruby-indent-action-line (boa)
  "Indent the current line in a Ruby action.
Argument BOA is the start position of the action."
  (narrow-to-region (1+ boa) (line-end-position))
  (ruby-indent-line)
  (widen)
  (unless (memq (char-after (line-beginning-position)) '(?\ ?\t))
    ;; no indentation -> considered top-level -> indentation can also be
    ;; performed by cc-mode
    (c-indent-line)))


;;;===========================================================================
;;;  Python
;;;===========================================================================

(declare-function python-indent-line "python")
(defvar prog-indentation-context)

(defvar antlr-python-language-mode-name "Python"
  "Value for `antlr-language-mode-name' when using language `antlr-python'.")

(defvar antlr-python-action-mode 'python-mode
  "Value for `antlr-action-mode' when using language `antlr-python'.")

(defvar antlr-python-init-submode #'antlr-init-python
  "Value for `antlr-init-submode' when using language `antlr-python'.")

(defvar antlr-python-action-font-lock-keywords
  '(antlr-no-action-keywords
    python-font-lock-keywords)
  "Value for `antlr-action-font-lock-keywords' when using language `antlr-python'.")

(defvar antlr-python-indent-action-line #'antlr-python-indent-action-line
  "Value for `antlr-indent-action-line' when using language `antlr-python'.")

(defun antlr-init-python ()
  "Initialize action language `antlr-python'."
  (require 'python))

(defun antlr-python-indent-action-line (boa)
  "Indent the current line in a Python action.
Argument BOA is the start position of the action."
  (let (leftouter)
    (save-excursion
      (goto-char boa)
      (setq leftouter (current-indentation))
      (forward-char)
      (skip-chars-forward " \t")
      (setq boa (and (eq (char-after) ?\n) (point))))
    ;; a multi-line Python action does not start in its own line: this is a bad
    ;; idea -> we do not touch those actions
    (when (and boa
               (eq antlr-indent-comment t) ; indent-region
               (boundp 'prog-indentation-context)) ; Emacs 24.5 or later
      ;; FIXME: These vars don't exist any more.
      (defvar syntax-ppss-cache) (defvar syntax-ppss-last)
      (let ((syntax-ppss-cache nil) ;#dynamic, in older Emacs...
            (syntax-ppss-last nil) ;#dynamic, in older Emacs...
            ;; TODO: do we also need to call `syntax-propertize' or
            ;; `syntax-ppss-flush-cache'?  and/or bind
            ;; `syntax-propertize-function'?
            (prog-indentation-context ;#dynamic
             (list (+ leftouter c-basic-offset) (list (1+ boa)))))
        (narrow-to-region (1+ boa) (point-max))
        (python-indent-line (eq this-command 'antlr-indent-command))))))


(provide 'antlr-mode)
;;; antlr-mode.el ends here
