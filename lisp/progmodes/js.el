;;; js.el --- Major mode for editing JavaScript  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2024 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dancol@dancol.org>
;; Maintainer: Daniel Colascione <dancol@dancol.org>
;; Version: 9
;; Date: 2009-07-25
;; Keywords: languages, javascript

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

;; This is based on Karl Landstrom's barebones javascript-mode.  This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;;
;; The main features of this JavaScript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments, and C preprocessor fontification.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "js-"; private names start with
;; "js--".

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))
(require 'newcomment)
(require 'imenu)
(require 'json)
(require 'prog-mode)
(require 'treesit)
(require 'c-ts-common) ; For comment indent and filling.

(eval-when-compile
  (require 'cl-lib)
  (require 'ido)
  (require 'rx))

(defvar ido-cur-list)
(defvar electric-layout-rules)
(declare-function ido-mode "ido" (&optional arg))
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-capture "treesit.c")

;;; Constants

(defconst js--name-start-re "[[:alpha:]_$]"
  "Regexp matching the start of a JavaScript identifier, without grouping.")

(defconst js--stmt-delim-chars "^;{}?:")

(defconst js--name-re (concat js--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a JavaScript identifier, without grouping.")

(defconst js--objfield-re (concat js--name-re ":")
  "Regexp matching the start of a JavaScript object field.")

(defconst js--dotted-name-re
  (concat js--name-re "\\(?:\\." js--name-re "\\)*")
  "Regexp matching a dot-separated sequence of JavaScript names.")

(defconst js--cpp-name-re js--name-re
  "Regexp matching a C preprocessor name.")

(defconst js--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst js--plain-method-re
  (concat "^\\s-*?\\(" js--dotted-name-re "\\)\\.prototype"
          "\\.\\(" js--name-re "\\)\\s-*?=\\s-*?\\(\\(?:async[ \t\n]+\\)function\\)\\_>")
  "Regexp matching an explicit JavaScript prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the `function' keyword.")

(defconst js--plain-class-re
  (concat "^\\s-*\\(" js--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching a JavaScript explicit prototype \"class\" declaration.
An example of this is \"Class.prototype = { method1: ...}\".")

;; var NewClass = BaseClass.extend(
(defconst js--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" js--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" js--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$"))

;; var NewClass = Class.create()
(defconst js--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" js--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()"))

(defconst js--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" js--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst js--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" js--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*("))

;; var NewClass = Class.create({
(defconst js--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" js--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" js--dotted-name-re "\\)\\s-*,\\s-*\\)?{?"))

;; Parent class name(s) (yes, multiple inheritance in JavaScript) are
;; matched with dedicated font-lock matchers
(defconst js--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" js--dotted-name-re "\\)"))

(defconst js--extjs-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" js--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" js--dotted-name-re "\\)")
  "Regexp matching an ExtJS class declaration (style 1).")

(defconst js--extjs-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" js--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" js--dotted-name-re "\\)")
  "Regexp matching an ExtJS class declaration (style 2).")

(defconst js--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" js--dotted-name-re "\\)")
  "Regexp matching a MochiKit class declaration.")

(defconst js--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst js--class-styles
  `((:name            "Plain"
     :class-decl      ,js--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       javascript)

    (:name            "MochiKit"
     :class-decl      ,js--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,js--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,js--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,js--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,js--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,js--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtJS (style 1)"
     :class-decl      ,js--extjs-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "ExtJS (style 2)"
     :class-decl      ,js--extjs-class-decl-re-2
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "Merrill Press"
     :class-decl      ,js--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "List of JavaScript class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class.  Its first group must match the name of its class.  If there
is a parent class, the second group should match, and it should be
the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name.  Otherwise, multiple definitions
will create multiple top-level entries.  Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains.")

(defconst js--available-frameworks
  (cl-loop for style in js--class-styles
           for framework = (plist-get style :framework)
           unless (memq framework available-frameworks)
           collect framework into available-frameworks
           finally return available-frameworks)
  "List of available JavaScript frameworks symbols.")

(defconst js--function-heading-1-re
  (concat
   "^\\s-*function\\(?:\\s-\\|\\*\\)+\\(" js--name-re "\\)")
  "Regexp matching the start of a JavaScript function header.
Match group 1 is the name of the function.")

(defconst js--function-heading-2-re
  (concat
   "^\\s-*\\(" js--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst js--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" js--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the JavaScript form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst js--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" js--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun js--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst js--keyword-re
  (js--regexp-opt-symbol
   '("abstract" "async" "await" "break" "case" "catch" "class" "const"
     "continue" "debugger" "default" "delete" "do" "else"
     "enum" "export" "extends" "final" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "native" "new" "of" "package"
     "private" "protected" "public" "return" "static"
     "super" "switch" "synchronized" "throw"
     "throws" "transient" "try" "typeof" "var" "void" "let"
     "yield" "volatile" "while" "with"))
  "Regexp matching any JavaScript keyword.")

(defconst js--basic-type-re
  (js--regexp-opt-symbol
   '("boolean" "byte" "char" "double" "float" "int" "long"
     "short" "void"))
  "Regular expression matching any predefined type in JavaScript.")

(defconst js--constant-re
  (js--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in JavaScript.")


(defconst js--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list js--function-heading-1-re 1 font-lock-function-name-face)
   (list js--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `js-mode'.")

(defconst js--font-lock-keywords-2
  (append js--font-lock-keywords-1
          (list (list js--keyword-re 1 font-lock-keyword-face)
                (cons js--basic-type-re font-lock-type-face)
                (cons js--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `js-mode'.")

;; js--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; js--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; js--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, js--pstate, is actually a list
;; of all js--pitem instances open after the marked character.
;;
;; The text property for b-end, js--pend, is simply the
;; js--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an js--pstate text property. Since no other
;; js--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; js--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at
;; subsequence parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(cl-defstruct (js--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `js--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `js--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst js--initial-pitem
  (make-js--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;;; User Customization

(defgroup js nil
  "Customization variables for JavaScript mode."
  :tag "JavaScript"
  :group 'languages)

(defcustom js-indent-level 4
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :safe 'integerp)

(defcustom js-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :safe 'integerp)

(defcustom js-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :safe 'integerp
  :version "24.1")

(defcustom js-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :safe 'integerp
  :version "24.1")

(defcustom js-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :safe 'integerp
  :version "24.1")

(defcustom js-switch-indent-offset 0
  "Number of additional spaces for indenting the contents of a switch block.
The value must not be negative."
  :type 'integer
  :safe 'integerp
  :version "24.4")

(defcustom js-flat-functions nil
  "Treat nested functions as top-level functions in `js-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean)

(defcustom js-indent-align-list-continuation t
  "Align continuation of non-empty ([{ lines in `js-mode'."
  :version "26.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom js-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `js-mode'."
  :type 'function)

(defcustom js-enabled-frameworks js--available-frameworks
  "Frameworks recognized by `js-mode'.
To improve performance, you may turn off some frameworks you
seldom use, either globally or on a per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           js--available-frameworks)))

(defvar js-js-switch-tabs (and (memq system-type '(darwin)) t)
  "Whether `js-mode' should display tabs while selecting them.
This is useful only if the windowing system has a good mechanism
for preventing Firefox from stealing the keyboard focus.")
(make-obsolete-variable 'js-js-switch-tabs "MozRepl no longer exists" "28.1")

(defvar js-js-tmpdir (locate-user-emacs-file "js/js")
  "Temporary directory used by `js-mode' to communicate with Mozilla.
This directory must be readable and writable by both Mozilla and Emacs.")
(make-obsolete-variable 'js-js-tmpdir "MozRepl no longer exists" "28.1")

(defvar js-js-timeout 5
  "Reply timeout for executing commands in Mozilla via `js-mode'.
The value is given in seconds.  Increase this value if you are
getting timeout messages.")
(make-obsolete-variable 'js-js-timeout "MozRepl no longer exists" "28.1")

(defcustom js-indent-first-init nil
  "Non-nil means specially indent the first variable declaration's initializer.
Normally, the first declaration's initializer is unindented, and
subsequent declarations have their identifiers aligned with it:

  var o = {
      foo: 3
  };

  var o = {
      foo: 3
  },
      bar = 2;

If this option has the value t, indent the first declaration's
initializer by an additional level:

  var o = {
          foo: 3
      };

  var o = {
          foo: 3
      },
      bar = 2;

If this option has the value `dynamic', if there is only one declaration,
don't indent the first one's initializer; otherwise, indent it.

  var o = {
      foo: 3
  };

  var o = {
          foo: 3
      },
      bar = 2;"
  :version "25.1"
  :type '(choice (const nil) (const t) (const dynamic))
  :safe 'symbolp)

(defcustom js-chain-indent nil
  "Use \"chained\" indentation.
Chained indentation applies when the current line starts with \".\".
If the previous expression also contains a \".\" at the same level,
then the \".\"s will be lined up:

  let x = svg.mumble()
             .chained;"
  :version "26.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom js-jsx-detect-syntax t
  "When non-nil, automatically detect whether JavaScript uses JSX.
`js-jsx-syntax' (which see) may be made buffer-local and set to
t.  The detection strategy can be customized by adding elements
to `js-jsx-regexps', which see."
  :version "27.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom js-jsx-syntax nil
  "When non-nil, parse JavaScript with consideration for JSX syntax.

This enables proper font-locking and indentation of code using
Facebook’s “JSX” syntax extension for JavaScript, for use with
Facebook’s “React” library.  Font-locking is like `sgml-mode'.
Indentation is also like `sgml-mode', although some indentation
behavior may differ slightly to align more closely with the
conventions of the React developer community.

When `js-mode' is already enabled, you should call
`js-jsx-enable' to set this variable.

It is set to be buffer-local (and t) when in `js-jsx-mode'."
  :version "27.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom js-jsx-align->-with-< t
  "When non-nil, “>” will be indented to the opening “<” in JSX.

When this is enabled, JSX indentation looks like this:

  <element
    attr=\"\"
  >
  </element>
  <input
  />

When this is disabled, JSX indentation looks like this:

  <element
    attr=\"\"
    >
  </element>
  <input
    />"
  :version "27.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom js-jsx-indent-level nil
  "When non-nil, indent JSX by this value, instead of like JS.

Let `js-indent-level' be 4.  When this variable is also set to
nil, JSX indentation looks like this (consistent):

  return (
      <element>
          <element>
              Hello World!
          </element>
      </element>
  )

Alternatively, when this variable is also set to 2, JSX
indentation looks like this (different):

  return (
      <element>
        <element>
          Hello World!
        </element>
      </element>
  )"
  :version "27.1"
  :type '(choice integer
                 (const :tag "Not Set" nil))
  :safe (lambda (x) (or (null x) (integerp x))))
;; This is how indentation behaved out-of-the-box until Emacs 27.  JSX
;; indentation was controlled with `sgml-basic-offset', which defaults
;; to 2, whereas `js-indent-level' defaults to 4.  Users who had the
;; same values configured for both their HTML and JS indentation would
;; luckily get consistent JSX indentation; most others were probably
;; unhappy.  I’d be surprised if anyone actually wants different
;; indentation levels, but just in case, here’s a way back to that.

(defcustom js-jsx-attribute-offset 0
  "Specifies a delta for JSXAttribute indentation.

Let `js-indent-level' be 2.  When this variable is also set to 0,
JSXAttribute indentation looks like this:

  <element
    attribute=\"value\">
  </element>

Alternatively, when this variable is also set to 2, JSXAttribute
indentation looks like this:

  <element
      attribute=\"value\">
  </element>

This variable is like `sgml-attribute-offset'."
  :version "27.1"
  :type 'integer
  :safe 'integerp)

;;; Keymap

(defvar-keymap js-mode-map
  :doc "Keymap for `js-mode'."
  "M-." #'js-find-symbol)

(defvar js-ts-mode-map (copy-keymap js-mode-map)
  "Keymap used in `js-ts-mode'.")

;;; Syntax table and parsing

(defvar js-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `js-mode' and `js-ts-mode'.")

(defvar-local js--quick-match-re nil
  "Autogenerated regexp used by `js-mode' to match buffer constructs.")

(defvar-local js--quick-match-re-func nil
  "Autogenerated regexp used by `js-mode' to match constructs and functions.")

(defvar-local js--cache-end 1
  "Last valid buffer position for the `js-mode' function cache.")

(defvar-local js--last-parse-pos nil
  "Latest parse position reached by `js--ensure-cache'.")

(defvar-local js--state-at-last-parse-pos nil
  "Parse state at `js--last-parse-pos'.")

(defun js--maybe-join (prefix separator suffix &rest list)
  "Helper function for `js--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (flatten-tree list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun js--update-quick-match-re ()
  "Internal function used by `js-mode' for caching buffer constructs.
This updates `js--quick-match-re', based on the current set of
enabled frameworks."
  (setq js--quick-match-re
        (js--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'extjs js-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype js-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (js--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*("

          (when (memq 'prototype js-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'extjs js-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress js-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo js-enabled-frameworks)
           "dojo\\.declare[ \t]*(")

         (when (memq 'mochikit js-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*(")

         ;; mumble.prototypeTHING
         (js--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'javascript js-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq js--quick-match-re-func
        (concat "function\\|" js--quick-match-re)))

(defun js--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun js--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst js--forward-pstate ()
  (js--forward-text-property 'js--pstate))

(defsubst js--backward-pstate ()
  (js--backward-text-property 'js--pstate))

(defun js--pitem-goto-h-end (pitem)
  (goto-char (js--pitem-h-begin pitem))
  (js--forward-pstate))

(defun js--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `js--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (js--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (line-end-position) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (js--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun js--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'js--re-search-backward-inner)
               ((> count 0) #'js--re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun js--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js--re-search-backward'."
  (let ((parse)
        (orig-macro-start
         (save-excursion
           (and (js--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((nth 8 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (js--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun js--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (js--re-search-forward regexp bound noerror (if count (- count) -1)))

(defun js--forward-expression ()
  "Move forward over a whole JavaScript expression.
This function doesn't move over expressions continued across
lines."
  (cl-loop
   ;; non-continued case; simplistic, but good enough?
   do (cl-loop until (or (eolp)
                         (progn
                           (forward-comment most-positive-fixnum)
                           (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
               do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (js--continued-expression-p)))))

(defun js--forward-function-decl ()
  "Move forward over a JavaScript function declaration.
This puts point at the `function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (unless (looking-at "\\(\\_<async\\_>[ \t\n]+\\)?\\_<function\\_>")
    (error "Invalid position"))
  (let ((name t))
    (goto-char (match-end 0))
    (forward-comment most-positive-fixnum)
    (when (eq (char-after) ?*)
      (forward-char)
      (forward-comment most-positive-fixnum))
    (when (looking-at js--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun js--function-prologue-beginning (&optional pos)
  "Return the start of the JavaScript function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at js--function-heading-2-re)
                  (looking-at js--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (let ((start nil))
        (and (or (looking-at "\\_<function\\_>")
                 (js--re-search-backward "\\_<function\\_>" nil t))
             (progn
               (setq start (match-beginning 0))
               (goto-char start)
               (when (looking-back "\\_<async\\_>[ \t\n]+" (- (point) 30))
                 (setq start (match-beginning 0)))
               (js--forward-function-decl))
             (<= pos (point))
             (or prologue-begin start))))))

(defun js--beginning-of-defun-raw ()
  "Helper function for `js-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (js--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (js--backward-pstate))
                (not (eq 'function (js--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun js--pstate-is-toplevel-defun (pstate)
  "Helper function for `js--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (cl-loop for pitem in pstate
           with func-depth = 0
           with func-pitem
           if (eq 'function (js--pitem-type pitem))
           do (cl-incf func-depth)
           and do (setq func-pitem pitem)
           finally return (if (eq func-depth 1) func-pitem)))

(defun js--beginning-of-defun-nested ()
  "Helper function for `js--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (cl-loop for pitem in (js--parse-state-at-point)
            if (and (eq 'function (js--pitem-type pitem))
                    (js--inside-pitem-p pitem))
            do (goto-char (js--pitem-h-begin pitem))
            and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (cl-loop for pstate = (js--backward-pstate)
            while pstate
            if (js--pstate-is-toplevel-defun pstate)
            do (goto-char (js--pitem-h-begin it))
            and return it)))

(defun js--beginning-of-defun-flat ()
  "Helper function for `js-beginning-of-defun'."
  (let ((pstate (js--beginning-of-defun-raw)))
    (when pstate
      (goto-char (js--pitem-h-begin (car pstate)))
      t)))

(defun js-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `js-mode'."
  (setq arg (or arg 1))
  (let ((found))
    (while (and (not (eobp)) (< arg 0))
      (cl-incf arg)
      (when (and (not js-flat-functions)
                 (or (eq (js-syntactic-context) 'function)
                     (js--function-prologue-beginning)))
        (js-end-of-defun))

      (if (js--re-search-forward
           "\\_<function\\_>" nil t)
          (progn (goto-char (js--function-prologue-beginning))
                 (setq found t))
        (goto-char (point-max))
        (setq found nil)))

    (while (> arg 0)
      (cl-decf arg)
      ;; If we're just past the end of a function, the user probably wants
      ;; to go to the beginning of *that* function
      (when (eq (char-before) ?})
        (backward-char))

      (let ((prologue-begin (js--function-prologue-beginning)))
        (cond ((and prologue-begin (< prologue-begin (point)))
               (goto-char prologue-begin)
               (setq found t))

              (js-flat-functions
               (setq found (js--beginning-of-defun-flat)))
              (t
               (when (js--beginning-of-defun-nested)
                 (setq found t))))))
    found))

(defun js--flush-caches (&optional beg _ignored)
  "Flush the `js-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq js--cache-end (min js--cache-end beg)))

(defmacro js--debug (&rest _arguments)
  ;; `(message ,@arguments)
  )

(defun js--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (js--pitem-paren-depth top-item))
      (cl-assert (not (get-text-property (1- (point)) 'js-pend)))
      (put-text-property (1- (point)) (point) 'js--pend top-item)
      (setf (js--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (js--pitem-add-child (cl-second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro js--ensure-cache--update-parse ()
  "Helper function for `js--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `js--ensure-cache'."
  '(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (js--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (cl-assert (> (nth 0 parse)
                            (js--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (js--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (js--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (js--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun js--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'js--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun js--split-name (string)
  "Split a JavaScript name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar js--guess-function-name-start nil)

(defun js--guess-function-name (position)
  "Guess the name of the JavaScript function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `js--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq js--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at js--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq js--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at js--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq js--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun js--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (js--forward-text-property
                             'js--pend))
        (setf (js--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(js--pstate t js--pend t)))

(defun js--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< js--cache-end limit)

    (c-save-buffer-state
        (open-items
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         goal-point)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (cl-loop for style in js--class-styles
                     if (memq (plist-get style :framework)
                              js-enabled-frameworks)
                     collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char js--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'js--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'js--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'js--pstate))
                (cl-assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list js--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (js--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (cl-loop while (re-search-forward js--quick-match-re-func nil t)
                   for orig-match-start = (goto-char (match-beginning 0))
                   for orig-match-end = (match-end 0)
                   do (js--ensure-cache--update-parse)
                   for orig-depth = (nth 0 parse)

                   ;; Each of these conditions should return non-nil if
                   ;; we should add a new item and leave point at the end
                   ;; of the new item's header (h-end in the
                   ;; js--pitem diagram). This point is the one
                   ;; after the last character we need to unambiguously
                   ;; detect this construct. If one of these evaluates to
                   ;; nil, the location of the point is ignored.
                   if (cond
                       ;; In comment or string
                       ((nth 8 parse) nil)

                       ;; Regular function declaration
                       ((and (looking-at "\\_<function\\_>")
                             (setq name (js--forward-function-decl)))
                        (when (eq name t)
                          (setq name (js--guess-function-name orig-match-end))
                          (if name
                              (when js--guess-function-name-start
                                (setq orig-match-start
                                      js--guess-function-name-start))

                            (setq name t)))

                        (cl-assert (eq (char-after) ?{))
                        (forward-char)
                        (save-excursion
                          (goto-char orig-match-start)
                          (when (looking-back "\\_<async\\_>[ \t\n]+"
                                              (- (point) 30))
                            (setq orig-match-start (match-beginning 0))))
                        (make-js--pitem
                         :paren-depth orig-depth
                         :h-begin orig-match-start
                         :type 'function
                         :name (if (eq name t)
                                   name
                                 (js--split-name name))))

                       ;; Macro
                       ((looking-at js--macro-decl-re)

                        ;; Macros often contain unbalanced parentheses.
                        ;; Make sure that h-end is at the textual end of
                        ;; the macro no matter what the parenthesis say.
                        (c-end-of-macro)
                        (js--ensure-cache--update-parse)

                        (make-js--pitem
                         :paren-depth (nth 0 parse)
                         :h-begin orig-match-start
                         :type 'macro
                         :name (list (match-string-no-properties 1))))

                       ;; "Prototype function" declaration
                       ((looking-at js--plain-method-re)
                        (goto-char (match-beginning 3))
                        (when (save-match-data
                                (js--forward-function-decl))
                          (forward-char)
                          (make-js--pitem
                           :paren-depth orig-depth
                           :h-begin orig-match-start
                           :type 'function
                           :name (nconc (js--split-name
                                         (match-string-no-properties 1))
                                        (list (match-string-no-properties 2))))))

                       ;; Class definition
                       ((cl-loop
                         with syntactic-context =
                         (js--syntactic-context-from-pstate open-items)
                         for class-style in filtered-class-styles
                         if (and (memq syntactic-context
                                       (plist-get class-style :contexts))
                                 (looking-at (plist-get class-style
                                                        :class-decl)))
                         do (goto-char (match-end 0))
                         and return
                         (make-js--pitem
                          :paren-depth orig-depth
                          :h-begin orig-match-start
                          :type class-style
                          :name (js--split-name
                                 (match-string-no-properties 1))))))

                   do (js--ensure-cache--update-parse)
                   and do (push it open-items)
                   and do (put-text-property
                           (1- (point)) (point) 'js--pstate open-items)
                   else do (goto-char orig-match-end))

          (goto-char limit)
          (js--ensure-cache--update-parse)
          (setq js--cache-end limit)
          (setq js--last-parse-pos limit)
          (setq js--state-at-last-parse-pos open-items)
          )))))

(defun js--end-of-defun-flat ()
  "Helper function for `js-end-of-defun'."
  (cl-loop while (js--re-search-forward "}" nil t)
           do (js--ensure-cache)
           if (get-text-property (1- (point)) 'js--pend)
           if (eq 'function (js--pitem-type it))
           return t
           finally do (goto-char (point-max))))

(defun js--end-of-defun-nested ()
  "Helper function for `js-end-of-defun'."
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (js--beginning-of-defun-nested))
                          (js--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (js--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (js--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun js-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `js-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (cl-incf arg)
    (js-beginning-of-defun)
    (js-beginning-of-defun)
    (unless (bobp)
      (js-end-of-defun)))

  (while (> arg 0)
    (cl-decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if js-flat-functions
        (js--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call js--end-of-defun-nested to do the real work
      (let ((prologue-begin (js--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (js--forward-function-decl)
               (forward-list))

              (t (js--end-of-defun-nested)))))))

(defun js--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at js--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun js--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `js-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (js--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (js--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun js--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `js-mode'."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;; Like (up-list -1), but only considers lists that end nearby"
(defun js--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our computation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun js--inside-param-list-p ()
  "Return non-nil if point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (js--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun js--inside-dojo-class-list-p ()
  "Return non-nil if point is in a Dojo multiple-inheritance class block."
  (ignore-errors
    (save-excursion
      (js--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at js--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

;;; Font Lock
(defun js--make-framework-matcher (framework &rest regexps)
  "Helper function for building `js--font-lock-keywords'.
Create a byte-compiled function for matching a concatenation of
REGEXPS, but only if FRAMEWORK is in `js-enabled-frameworks'."
  (let ((regexp (apply #'concat regexps)))
    (lambda (limit)
      (when (memq framework js-enabled-frameworks)
        (re-search-forward regexp limit t)))))

(defvar-local js--tmp-location nil)

(defun js--forward-destructuring-spec (&optional func)
  "Move forward over a JavaScript destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true if this was actually a
spec.  FUNC must preserve the match data."
  (pcase (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (js--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at js--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at js--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (js--forward-destructuring-spec func))
                      ((looking-at js--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun js--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at js--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (js--forward-destructuring-spec))

                          (js--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (js--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

;; It wouldn’t be sufficient to font-lock JSX with mere regexps, since
;; a JSXElement may be nested inside a JS expression within the
;; boundaries of a parent JSXOpeningElement, and such a hierarchy
;; ought to be fontified like JSX, JS, and JSX respectively:
;;
;;   <div attr={void(<div></div>) && void(0)}></div>
;;
;;   <div attr={           ← JSX
;;          void(          ← JS
;;            <div></div>  ← JSX
;;          ) && void(0)   ← JS
;;        }></div>         ← JSX
;;
;; `js-syntax-propertize' unambiguously identifies JSX syntax,
;; including when it’s nested.
;;
;; Using a matcher function for each relevant part, retrieve match
;; data recorded as syntax properties for fontification.

(defconst js-jsx--font-lock-keywords
  `((js-jsx--match-tag-name 0 font-lock-function-name-face t)
    (js-jsx--match-attribute-name 0 font-lock-variable-name-face t)
    (js-jsx--match-text 0 'default t) ; “Undo” keyword fontification.
    (js-jsx--match-tag-beg)
    (js-jsx--match-tag-end)
    (js-jsx--match-expr))
  "JSX font lock faces and multiline text properties.")

(defun js-jsx--match-tag-name (limit)
  "Match JSXBoundaryElement names, until LIMIT."
  (when js-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'js-jsx-tag-name nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'js-jsx-tag-name))
                 (progn (set-match-data value) t))
            (js-jsx--match-tag-name limit))))))

(defun js-jsx--match-attribute-name (limit)
  "Match JSXAttribute names, until LIMIT."
  (when js-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'js-jsx-attribute-name nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'js-jsx-attribute-name))
                 (progn (set-match-data value) t))
            (js-jsx--match-attribute-name limit))))))

(defun js-jsx--match-text (limit)
  "Match JSXText, until LIMIT."
  (when js-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'js-jsx-text nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'js-jsx-text))
                 (progn (set-match-data value)
                        (put-text-property (car value) (cadr value) 'font-lock-multiline t)
                        t))
            (js-jsx--match-text limit))))))

(defun js-jsx--match-tag-beg (limit)
  "Match JSXBoundaryElements from start, until LIMIT."
  (when js-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'js-jsx-tag-beg nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'js-jsx-tag-beg))
                 (progn (put-text-property pos (cdr value) 'font-lock-multiline t) t))
            (js-jsx--match-tag-beg limit))))))

(defun js-jsx--match-tag-end (limit)
  "Match JSXBoundaryElements from end, until LIMIT."
  (when js-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'js-jsx-tag-end nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'js-jsx-tag-end))
                 (progn (put-text-property value pos 'font-lock-multiline t) t))
            (js-jsx--match-tag-end limit))))))

(defun js-jsx--match-expr (limit)
  "Match JSXExpressionContainers, until LIMIT."
  (when js-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'js-jsx-expr nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'js-jsx-expr))
                 (progn (put-text-property pos value 'font-lock-multiline t) t))
            (js-jsx--match-expr limit))))))

(defconst js--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@js--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (js--class-decl-matcher
     ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (js--class-decl-matcher
     ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq js--tmp-location (match-end 2))
           (goto-char js--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq js--tmp-location nil)
       (goto-char (line-end-position)))
     (when js--tmp-location
       (save-excursion
         (goto-char js--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (js--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" js--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" js--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" js--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" js--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*" js--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" js--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (js--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" js--basic-type-re)
      (list #'js--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" js--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" js--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" js--name-re "\\)?\\s-*(\\s-*"
       js--name-start-re)
      (list (concat "\\(" js--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" js--name-re "\\s-*[,)]")
      (list js--name-re
            '(if (save-excursion (backward-char)
                                 (js--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face)))

    ;; jsx (when enabled)
    ,@js-jsx--font-lock-keywords)
  "Level three font lock for `js-mode'.")

(defun js--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (js--ensure-cache)
  (cl-assert (js--pitem-h-begin pitem))
  (cl-assert (js--pitem-paren-depth pitem))

  (and (> (point) (js--pitem-h-begin pitem))
       (or (null (js--pitem-b-end pitem))
           (> (js--pitem-b-end pitem) (point)))))

(defun js--parse-state-at-point ()
  "Parse the JavaScript program state at point.
Return a list of `js--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (js--ensure-cache)
      (let ((pstate (or (save-excursion
                          (js--backward-pstate))
                        (list js--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (cl-loop for pitem = (car pstate)
                 until (or (eq (js--pitem-type pitem)
                               'toplevel)
                           (js--inside-pitem-p pitem))
                 do (pop pstate))

        pstate))))

(defun js--syntactic-context-from-pstate (pstate)
  "Return the JavaScript syntactic context corresponding to PSTATE."
  (let ((type (js--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun js-syntactic-context ()
  "Return the JavaScript syntactic context at point.
When called interactively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (js--syntactic-context-from-pstate
                             (js--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun js--class-decl-matcher (limit)
  "Font lock function used by `js-mode'.
This performs fontification according to `js--class-styles'."
  (when js-enabled-frameworks
    (cl-loop initially (js--ensure-cache limit)
             while (re-search-forward js--quick-match-re limit t)
             for orig-end = (match-end 0)
             do (goto-char (match-beginning 0))
             if (cl-loop for style in js--class-styles
                         for decl-re = (plist-get style :class-decl)
                         if (and (memq (plist-get style :framework)
                                       js-enabled-frameworks)
                                 (memq (js-syntactic-context)
                                       (plist-get style :contexts))
                                 decl-re
                                 (looking-at decl-re))
                         do (goto-char (match-end 0))
                         and return t)
             return t
             else do (goto-char orig-end))))

(defconst js--font-lock-keywords
  '(js--font-lock-keywords-3 js--font-lock-keywords-1
                                   js--font-lock-keywords-2
                                   js--font-lock-keywords-3)
  "Font lock keywords for `js-mode'.  See `font-lock-keywords'.")

(defun js-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      font-lock-string-face
    (if (save-excursion
          (goto-char (nth 8 state))
          (looking-at "/\\*\\*"))
        font-lock-doc-face
      font-lock-comment-face)))

(defconst js--syntax-propertize-regexp-regexp
  (rx
   ;; Start of regexp.
   "/"
   (0+ (or
        ;; Match characters outside of a character class.
        (not (any ?\[ ?/ ?\\))
        ;; Match backslash quoted characters.
        (and "\\" not-newline)
        ;; Match character class.
        (and
         "["
         (0+ (or
              (not (any ?\] ?\\))
              (and "\\" not-newline)))
         "]")))
   (group (zero-or-one "/")))
  "Regular expression matching a JavaScript regexp literal.")

(defun js-syntax-propertize-regexp (end)
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      ;; A /.../ regexp.
      (goto-char (nth 8 ppss))
      (when (looking-at js--syntax-propertize-regexp-regexp)
        ;; Don't touch text after END.
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defconst js--unary-keyword-re
  (js--regexp-opt-symbol '("await" "delete" "typeof" "void" "yield"))
  "Regexp matching unary operator keywords.")

(defun js--unary-keyword-p (string)
  "Check if STRING is a unary operator keyword in JavaScript."
  (string-match-p js--unary-keyword-re string))

;; Adding `syntax-multiline' text properties to JSX isn’t sufficient
;; to identify multiline JSX when first typing it.  For instance, if
;; the user is typing a JSXOpeningElement for the first time…
;;
;;   <div
;;       ^ (point)
;;
;; …and the user inserts a line break after the tag name (before the
;; JSXOpeningElement starting on that line has been unambiguously
;; identified as such), then the `syntax-propertize' region won’t be
;; extended backwards to the start of the JSXOpeningElement:
;;
;;   <div         ← This line wasn't JSX when last edited.
;;     attr="">   ← Despite completing the JSX, the next
;;             ^    `syntax-propertize' region wouldn’t magically
;;                  extend back a few lines.
;;
;; Therefore, to try and recover from this scenario, parse backward
;; from “>” to try and find the start of JSXBoundaryElements, and
;; extend the `syntax-propertize' region there.

(defun js--syntax-propertize-extend-region (start end)
  "Extend the START-END region for propertization, if necessary.
For use by `syntax-propertize-extend-region-functions'."
  (if js-jsx-syntax (js-jsx--syntax-propertize-extend-region start end)))

(defun js-jsx--syntax-propertize-extend-region (start end)
  "Extend the START-END region for propertization, if necessary.
If any “>” in the region appears to be the end of a tag starting
before the start of the region, extend region backwards to the
start of that tag so parsing may proceed from that point.
For use by `syntax-propertize-extend-region-functions'."
  (let (new-start
        forward-sexp-function ; Use the Lisp version.
        parse-sexp-lookup-properties) ; Fix backward-sexp error here.
    (catch 'stop
      (goto-char start)
      (while (re-search-forward ">" end t)
        (catch 'continue
          ;; Check if this is really a right shift bitwise operator
          ;; (“>>” or “>>>”).
          (unless (or (eq (char-before (1- (point))) ?>)
                      (eq (char-after) ?>))
            (save-excursion
              (backward-char)
              (while (progn (if (= (point) (point-min)) (throw 'continue nil))
                            (/= (char-before) ?<))
                (skip-chars-backward " \t\n")
                (if (= (point) (point-min)) (throw 'continue nil))
                (cond
                 ((memq (char-before) '(?\" ?\' ?\` ?\}))
                  (condition-case nil
                      (backward-sexp)
                    (scan-error (throw 'continue nil))))
                 ((memq (char-before) '(?\/ ?\=)) (backward-char))
                 ((looking-back js--dotted-name-re (line-beginning-position) t)
                  (goto-char (match-beginning 0)))
                 (t (throw 'continue nil))))
              (when (< (point) start)
                (setq new-start (1- (point)))
                (throw 'stop nil)))))))
    (if new-start (cons new-start end))))

;; When applying syntax properties, since `js-syntax-propertize' uses
;; `syntax-propertize-rules' to parse JSXBoundaryElements iteratively
;; and statelessly, whenever we exit such an element, we need to
;; determine the JSX depth.  If >0, then we know to apply syntax
;; properties to JSXText up until the next JSXBoundaryElement occurs.
;; But if the JSX depth is 0, then—importantly—we know to NOT parse
;; the following code as JSXText, rather propertize it as regular JS
;; as long as warranted.
;;
;; Also, when indenting code, we need to know if the code we’re trying
;; to indent is on the 2nd or later line of multiline JSX, in which
;; case the code is indented according to XML-like JSX conventions.
;;
;; For the aforementioned reasons, we find ourselves needing to
;; determine whether point is enclosed in JSX or not; and, if so,
;; where the JSX is.  The following functions provide that knowledge.

(defconst js-jsx--tag-start-re
  (concat "\\(" js--dotted-name-re "\\)\\(?:"
          ;; Whitespace is only necessary if an attribute implies JSX.
          "\\(?:\\s-\\|\n\\)*[{/>]"
          "\\|"
          "\\(?:\\s-\\|\n\\)+" js--name-start-re
          "\\)")
  "Regexp unambiguously matching a JSXOpeningElement.")

(defun js-jsx--matched-tag-type ()
  "Determine if the last “<” was a JSXBoundaryElement and its type.
Return `close' for a JSXClosingElement/JSXClosingFragment match,
return `self-closing' for some self-closing JSXOpeningElements,
else return `other'."
  (cond
   ((= (char-after) ?/) (forward-char) 'close) ; JSXClosingElement/JSXClosingFragment
   ((= (char-after) ?>) (forward-char) 'other) ; JSXOpeningFragment
   ((and (looking-at js-jsx--tag-start-re) ; JSXOpeningElement
         (not (js--unary-keyword-p (match-string 1))))
    (goto-char (match-end 0))
    (if (= (char-before) ?/) 'self-closing 'other))))

(defconst js-jsx--self-closing-re "/\\s-*>"
  "Regexp matching the end of a self-closing JSXOpeningElement.")

(defun js-jsx--matching-close-tag-pos ()
  "Return position of the closer of the opener before point.
Assuming a JSXOpeningElement or a JSXOpeningFragment is
immediately before point, find a matching JSXClosingElement or
JSXClosingFragment, skipping over any nested JSXElements to find
the match.  Return nil if a match can’t be found."
  (let ((tag-stack 1) tag-pos type last-pos pos)
    (catch 'stop
      (while (and (re-search-forward "<\\s-*" nil t) (not (eobp)))
        ;; Not inside a comment or string.
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (when (setq tag-pos (match-beginning 0)
                      type (js-jsx--matched-tag-type))
            (when last-pos
              (setq pos (point))
              (goto-char last-pos)
              (while (re-search-forward js-jsx--self-closing-re pos 'move)
                (setq tag-stack (1- tag-stack))))
            (if (eq type 'close)
                (progn
                  (setq tag-stack (1- tag-stack))
                  (when (= tag-stack 0)
                    (throw 'stop tag-pos)))
              ;; JSXOpeningElements that we know are self-closing
              ;; aren’t added to the stack at all (because point is
              ;; already past that syntax).
              (unless (eq type 'self-closing)
                (setq tag-stack (1+ tag-stack))))
            (setq last-pos (point))))))))

(defun js-jsx--enclosing-tag-pos ()
  "Return beginning and end of a JSXElement about point.
Look backward for a JSXElement that both starts before point and
also ends at/after point.  That may be either a self-closing
JSXElement or a JSXOpeningElement/JSXClosingElement pair."
  (let ((start (point)) tag-beg tag-beg-pos tag-end-pos close-tag-pos)
    (while
        (and
         (setq tag-beg (js--backward-text-property 'js-jsx-tag-beg))
         (progn
           (setq tag-beg-pos (point)
                 tag-end-pos (cdr tag-beg))
           (not
            (or
             (and (eq (car tag-beg) 'self-closing)
                  (< start tag-end-pos))
             (and (eq (car tag-beg) 'open)
                  (or (< start tag-end-pos)
                      (progn
                        (unless
                            ;; Try to read a cached close position,
                            ;; but it might not be available yet.
                            (setq close-tag-pos
                                  (get-text-property (point) 'js-jsx-close-tag-pos))
                          (save-excursion
                            (goto-char tag-end-pos)
                            (setq close-tag-pos (js-jsx--matching-close-tag-pos)))
                          (when close-tag-pos
                            ;; Cache the close position to make future
                            ;; searches faster.
                            (put-text-property
                             (point) (1+ (point))
                             'js-jsx-close-tag-pos close-tag-pos)))
                        ;; The JSXOpeningElement may be unclosed, else
                        ;; the closure must occur at/after the start
                        ;; point (otherwise, a miscellaneous previous
                        ;; JSXOpeningElement has been found, so keep
                        ;; looking backwards for an enclosing one).
                        (or (not close-tag-pos) (<= start close-tag-pos)))))))))
      ;; Don't return the last tag pos, as it wasn't enclosing.
      (setq tag-beg nil close-tag-pos nil))
    (and tag-beg (list tag-beg-pos tag-end-pos close-tag-pos))))

(defun js-jsx--at-enclosing-tag-child-p ()
  "Return t if point is at an enclosing tag’s child."
  (let ((pos (save-excursion (js-jsx--enclosing-tag-pos))))
    (and pos (>= (point) (nth 1 pos)))))

;; We implement `syntax-propertize-function' logic fully parsing JSX
;; in order to provide very accurate JSX indentation, even in the most
;; complex cases (e.g. to indent JSX within a JS expression within a
;; JSXAttribute…), as over the years users have requested this.  Since
;; we find so much information during this parse, we later use some of
;; the useful bits for font-locking, too.
;;
;; Some extra effort is devoted to ensuring that no code which could
;; possibly be valid JS is ever misinterpreted as partial JSX, since
;; that would be regressive.
;;
;; We first parse trying to find the minimum number of components
;; necessary to unambiguously identify a JSXBoundaryElement, even if
;; it is a partial one.  If a complete one is parsed, we move on to
;; parse any JSXText.  When that’s terminated, we unwind back to the
;; `syntax-propertize-rules' loop so the next JSXBoundaryElement can
;; be parsed, if any, be it an opening or closing one.

(defun js-jsx--text-range (beg end)
  "Identify JSXText within a “>/{/}/<” pair."
  (when (> (- end beg) 0)
    (save-excursion
      (goto-char beg)
      (while (and (skip-chars-forward " \t\n" end) (< (point) end))
        ;; Comments and string quotes don’t serve their usual
        ;; syntactic roles in JSXText; make them plain punctuation to
        ;; negate those roles.
        (when (or (= (char-after) ?/) ; comment
                  (= (syntax-class (syntax-after (point))) 7)) ; string quote
          (put-text-property (point) (1+ (point)) 'syntax-table '(1)))
        (forward-char)))
    ;; Mark JSXText so it can be font-locked as non-keywords.
    (put-text-property beg (1+ beg) 'js-jsx-text (list beg end (current-buffer)))
    ;; Ensure future propertization beginning from within the
    ;; JSXText determines JSXText context from earlier lines.
    (put-text-property beg end 'syntax-multiline t)))

;; In order to respect the end boundary `syntax-propertize-function'
;; sets, care is taken in the following functions to abort parsing
;; whenever that boundary is reached.

(defun js-jsx--syntax-propertize-tag-text (end)
  "Determine if JSXText is before END and propertize it.
Text within an open/close tag pair may be JSXText.  Temporarily
interrupt JSXText by JSXExpressionContainers, and terminate
JSXText when another JSXBoundaryElement is encountered.  Despite
terminations, all JSXText will be identified once all the
JSXBoundaryElements within an outermost JSXElement’s tree have
been propertized."
  (let ((text-beg (point))
        forward-sexp-function) ; Use Lisp version.
    (catch 'stop
      (while (re-search-forward "[{<]" end t)
        (js-jsx--text-range text-beg (1- (point)))
        (cond
         ((= (char-before) ?{)
          (let (expr-beg expr-end)
            (condition-case nil
                (save-excursion
                  (backward-char)
                  (setq expr-beg (point))
                  (forward-sexp)
                  (setq expr-end (point)))
              (scan-error nil))
            ;; Recursively propertize the JSXExpressionContainer’s
            ;; (possibly-incomplete) expression.
            (js-syntax-propertize (1+ expr-beg) (if expr-end (min (1- expr-end) end) end))
            ;; Ensure future propertization beginning from within the
            ;; (possibly-incomplete) expression can determine JSXText
            ;; context from earlier lines.
            (put-text-property expr-beg (1+ expr-beg) 'js-jsx-expr (or expr-end end)) ; font-lock
            (put-text-property expr-beg (if expr-end (min expr-end end) end) 'syntax-multiline t) ; syntax-propertize
            ;; Exit the JSXExpressionContainer if that’s possible,
            ;; else move to the end of the propertized area.
            (goto-char (if expr-end (min expr-end end) end))))
         ((= (char-before) ?<)
          (backward-char) ; Ensure the next tag can be propertized.
          (throw 'stop nil)))
        (setq text-beg (point))))))

(defconst js-jsx--attribute-name-re (concat js--name-start-re
                                            "\\(?:\\s_\\|\\sw\\|-\\)*")
  "Like `js--name-re', but matches “-” as well.")

(defun js-jsx--syntax-propertize-tag (end)
  "Determine if a JSXBoundaryElement is before END and propertize it.
Disambiguate JSX from inequality operators and arrow functions by
testing for syntax only valid as JSX."
  (let ((tag-beg (1- (point))) tag-end (type 'open)
        name-beg name-match-data expr-attribute-beg unambiguous
        forward-sexp-function) ; Use Lisp version.
    (catch 'stop
      (while (and (< (point) end)
                  (progn (skip-chars-forward " \t\n" end)
                         (< (point) end)))
        (cond
         ((= (char-after) ?>)
          ;; Make the closing “>” a close parenthesis.
          (put-text-property (point) (1+ (point)) 'syntax-table
                             (eval-when-compile (string-to-syntax ")<")))
          (forward-char)
          (setq unambiguous t)
          (throw 'stop nil))
         ;; Handle a JSXSpreadChild (“<Foo {...bar}”) or a
         ;; JSXExpressionContainer as a JSXAttribute value
         ;; (“<Foo bar={…}”).  Check this early in case continuing a
         ;; JSXAttribute parse.
         ((or (and name-beg (= (char-after) ?{))
              (setq expr-attribute-beg nil))
          (setq unambiguous t) ; JSXExpressionContainer post tag name ⇒ JSX
          (when expr-attribute-beg
            ;; Remember that this JSXExpressionContainer is part of a
            ;; JSXAttribute, as that can affect its expression’s
            ;; indentation.
            (put-text-property
             (point) (1+ (point)) 'js-jsx-expr-attribute expr-attribute-beg)
            (setq expr-attribute-beg nil))
          (let (expr-end)
            (condition-case nil
                (save-excursion
                  (forward-sexp)
                  (setq expr-end (point)))
              (scan-error nil))
            (forward-char)
            (if (>= (point) end) (throw 'stop nil))
            (skip-chars-forward " \t\n" end)
            (if (>= (point) end) (throw 'stop nil))
            (if (= (char-after) ?}) (forward-char) ; Shortcut to bail.
              ;; Recursively propertize the JSXExpressionContainer’s
              ;; expression.
              (js-syntax-propertize (point) (if expr-end (min (1- expr-end) end) end))
              ;; Exit the JSXExpressionContainer if that’s possible,
              ;; else move to the end of the propertized area.
              (goto-char (if expr-end (min expr-end end) end)))))
         ((= (char-after) ?/)
          ;; Assume a tag is an open tag until a slash is found, then
          ;; figure out what type it actually is.
          (if (eq type 'open) (setq type (if name-beg 'self-closing 'close)))
          (forward-char))
         ((and (not name-beg) (looking-at js--dotted-name-re))
          ;; Don’t match code like “if (i < await foo)”
          (if (js--unary-keyword-p (match-string 0)) (throw 'stop nil))
          ;; Save boundaries for later fontification after
          ;; unambiguously determining the code is JSX.
          (setq name-beg (match-beginning 0)
                name-match-data (match-data))
          (goto-char (match-end 0)))
         ((and name-beg (looking-at js-jsx--attribute-name-re))
          (setq unambiguous t) ; Non-unary name followed by 2nd name ⇒ JSX
          ;; Save JSXAttribute’s name’s match data for font-locking later.
          (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                             'js-jsx-attribute-name (match-data))
          (goto-char (match-end 0))
          (if (>= (point) end) (throw 'stop nil))
          (skip-chars-forward " \t\n" end)
          (if (>= (point) end) (throw 'stop nil))
          ;; “=” is optional for null-valued JSXAttributes.
          (when (= (char-after) ?=)
            (forward-char)
            (if (>= (point) end) (throw 'stop nil))
            (skip-chars-forward " \t\n" end)
            (if (>= (point) end) (throw 'stop nil))
            ;; Skip over strings (if possible).  Any
            ;; JSXExpressionContainer here will be parsed in the
            ;; next iteration of the loop.
            (if (memq (char-after) '(?\" ?\' ?\`))
                (progn
                  ;; Record the string’s position so derived modes
                  ;; applying syntactic fontification atypically
                  ;; (e.g. js2-mode) can recognize it as part of JSX.
                  (put-text-property (point) (1+ (point)) 'js-jsx-string t)
                  (condition-case nil
                      (forward-sexp)
                    (scan-error (throw 'stop nil))))
              ;; Save JSXAttribute’s beginning in case we find a
              ;; JSXExpressionContainer as the JSXAttribute’s value which
              ;; we should associate with the JSXAttribute.
              (setq expr-attribute-beg (match-beginning 0)))))
         ;; There is nothing more to check; this either isn’t JSX, or
         ;; the tag is incomplete.
         (t (throw 'stop nil)))))
    (when unambiguous
      ;; Save JSXBoundaryElement’s name’s match data for font-locking.
      (if name-beg (put-text-property name-beg (1+ name-beg) 'js-jsx-tag-name name-match-data))
      ;; Make the opening “<” an open parenthesis.
      (put-text-property tag-beg (1+ tag-beg) 'syntax-table
                         (eval-when-compile (string-to-syntax "(>")))
      ;; Prevent “out of range” errors when typing at the end of a buffer.
      (setq tag-end (if (eobp) (1- (point)) (point)))
      ;; Mark beginning and end of tag for font-locking.
      (put-text-property tag-beg (1+ tag-beg) 'js-jsx-tag-beg (cons type tag-end))
      (put-text-property tag-end (1+ tag-end) 'js-jsx-tag-end tag-beg)
      ;; Use text properties to extend the syntax-propertize region
      ;; backward to the beginning of the JSXBoundaryElement in the
      ;; future.  Typically the closing angle bracket could suggest
      ;; extending backward, but that would also involve more rigorous
      ;; parsing, and the closing angle bracket may not even exist yet
      ;; if the JSXBoundaryElement is still being typed.
      (put-text-property tag-beg (1+ tag-end) 'syntax-multiline t))
    (if (js-jsx--at-enclosing-tag-child-p) (js-jsx--syntax-propertize-tag-text end))))

(defconst js-jsx--text-properties
  (list
   'js-jsx-tag-beg nil 'js-jsx-tag-end nil 'js-jsx-close-tag-pos nil
   'js-jsx-tag-name nil 'js-jsx-attribute-name nil 'js-jsx-string nil
   'js-jsx-text nil 'js-jsx-expr nil 'js-jsx-expr-attribute nil)
  "Plist of text properties added by `js-syntax-propertize'.")

(defun js-syntax-propertize (start end)
  ;; JavaScript allows immediate regular expression objects, written /.../.
  (goto-char start)
  (if js-jsx-syntax (remove-text-properties start end js-jsx--text-properties))
  (js-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ;; Distinguish /-division from /-regexp chars (and from /-comment-starter).
    ;; FIXME: Allow regexps after infix ops like + ...
    ;; https://developer.mozilla.org/en/JavaScript/Reference/Operators
    ;; We can probably just add +, -, <, >, %, ^, ~, ?, : at which
    ;; point I think only * and / would be missing which could also be added,
    ;; but need care to avoid affecting the // and */ comment markers.
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
	 (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   ;; If the / is at the beginning of line, we have to check
                   ;; the end of the previous text.
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (js-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b"))
    ("<" (0 (ignore
             (when js-jsx-syntax
               ;; Not inside a comment or string.
               (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
                 (js-jsx--syntax-propertize-tag end)))))))
   (point) end))

(defconst js--prettify-symbols-alist
  '(("=>" . ?⇒)
    (">=" . ?≥)
    ("<=" . ?≤))
  "Alist of symbol prettifications for JavaScript.")

;;; Indentation

(defconst js--possibly-braceless-keyword-re
  (js--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst js--declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
  "Regular expression matching variable declaration keywords.")

(defconst js--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\|"
          (js--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")

(defun js-jsx--looking-at-start-tag-p ()
  "Non-nil if a JSXOpeningElement immediately follows point."
  (let ((tag-beg (get-text-property (point) 'js-jsx-tag-beg)))
    (and tag-beg (memq (car tag-beg) '(open self-closing)))))

(defun js--looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js--indent-operator-re)
         (or (not (eq (char-after) ?:))
             (save-excursion
               (js--backward-syntactic-ws)
               (when (= (char-before) ?\)) (backward-list))
               (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (eq (char-after) ??))))
         (not (and
               (eq (char-after) ?/)
               (save-excursion
                 (eq (nth 3 (syntax-ppss)) ?/))))
         (not (and
               (eq (char-after) ?*)
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" js--name-re " *(\\)"))
               (save-excursion
                 (js--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{)))))
         ;; “<” isn’t necessarily an operator in JSX.
         (not (and js-jsx-syntax (js-jsx--looking-at-start-tag-p))))))

(defun js--find-newline-backward ()
  "Move backward to the nearest newline that is not in a block comment."
  (let ((continue t)
        (result t))
    (while continue
      (setq continue nil)
      (if (search-backward "\n" nil t)
          (let ((parse (syntax-ppss)))
            ;; We match the end of a // comment but not a newline in a
            ;; block comment.
            (when (nth 4 parse)
              (goto-char (nth 8 parse))
              ;; If we saw a block comment, keep trying.
              (unless (nth 7 parse)
                (setq continue t))))
        (setq result nil)))
    result))

(defun js-jsx--looking-back-at-end-tag-p ()
  "Non-nil if a JSXClosingElement immediately precedes point."
  (get-text-property (point) 'js-jsx-tag-end))

(defun js--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (js--looking-at-operator-p)
        (if (eq (char-after) ?/)
            (prog1
                (not (nth 3 (syntax-ppss (1+ (point)))))
              (forward-char -1))
          (or
           (not (memq (char-after) '(?- ?+)))
           (progn
             (forward-comment (- (point)))
             (not (memq (char-before) '(?, ?\[ ?\())))))
      (and (js--find-newline-backward)
           (progn
             (skip-chars-backward " \t")
             (and
              ;; The “>” at the end of any JSXBoundaryElement isn’t
              ;; part of a continued expression.
              (not (and js-jsx-syntax (js-jsx--looking-back-at-end-tag-p)))
              (progn
                (or (bobp) (backward-char))
                (and (> (point) (point-min))
                     (save-excursion
                       (backward-char)
                       (not (looking-at "[/*]/\\|=>")))
                     (js--looking-at-operator-p)
                     (and (progn (backward-char)
                                 (not (looking-at "\\+\\+\\|--\\|/[/*]"))))))))))))

(defun js--skip-term-backward ()
  "Skip a term before point; return t if a term was skipped."
  (let ((term-skipped nil))
    ;; Skip backward over balanced parens.
    (let ((progress t))
      (while progress
        (setq progress nil)
        ;; First skip whitespace.
        (skip-syntax-backward " ")
        ;; Now if we're looking at closing paren, skip to the opener.
        ;; This doesn't strictly follow JS syntax, in that we might
        ;; skip something nonsensical like "()[]{}", but it is enough
        ;; if it works ok for valid input.
        (when (memq (char-before) '(?\] ?\) ?\}))
          (setq progress t term-skipped t)
          (backward-list))))
    ;; Maybe skip over a symbol.
    (let ((save-point (point)))
      (if (and (< (skip-syntax-backward "w_") 0)
                 (looking-at js--name-re))
          ;; Skipped.
          (progn
            (setq term-skipped t)
            (skip-syntax-backward " "))
        ;; Did not skip, so restore point.
        (goto-char save-point)))
    (when (and term-skipped (> (point) (point-min)))
      (backward-char)
      (eq (char-after) ?.))))

(defun js--skip-terms-backward ()
  "Skip any number of terms backward.
Move point to the earliest \".\" without changing paren levels.
Returns t if successful, nil if no term was found."
  (when (js--skip-term-backward)
    ;; Found at least one.
    (let ((last-point (point)))
      (while (js--skip-term-backward)
        (setq last-point (point)))
      (goto-char last-point)
      t)))

(defun js--chained-expression-p ()
  "A helper for js--proper-indentation that handles chained expressions.
A chained expression is when the current line starts with '.' and the
previous line also has a '.' expression.
This function returns the indentation for the current line if it is
a chained expression line; otherwise nil.
This should only be called while point is at the start of the line's content,
as determined by `back-to-indentation'."
  (when js-chain-indent
    (save-excursion
      (when (and (eq (char-after) ?.)
                 (js--continued-expression-p)
                 (js--find-newline-backward)
                 (js--skip-terms-backward))
        (current-column)))))

(defun js--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward " \t\n}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
          (js--re-search-backward "\\_<do\\_>" (line-beginning-position) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (js--re-search-forward
                           "\\_<while\\_>" (line-end-position) t))
		     (= (current-indentation) saved-indent)))))))))


(defun js--ctrl-statement-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (line-beginning-position) (point-min)))
                 (not (looking-at "[{]"))
                 (js--re-search-backward "[[:graph:]]" nil t)
                 (progn
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at js--possibly-braceless-keyword-re))
                 (memq (char-before) '(?\s ?\t ?\n ?\}))
                 (not (js--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) js-indent-level)))))

(defun js--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c js-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun js--same-line (pos)
  (and (>= pos (line-beginning-position))
       (<= pos (line-end-position))))

(defun js--multi-line-declaration-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (forward-sexp-function ; Use Lisp version.
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js--declaration-keyword-re))
        (let ((pt (point)))
          (when (looking-at js--indent-operator-re)
            (goto-char (match-end 0)))
          ;; The "operator" is probably a regexp literal opener.
          (when (nth 3 (syntax-ppss))
            (goto-char pt)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at js--indent-operator-re)
                                   (js--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (js--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js--declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun js--indent-in-array-comp (bracket)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (save-excursion
      (goto-char bracket)
      (when (looking-at "\\[")
        (forward-char 1)
        (js--forward-syntactic-ws)
        (if (looking-at "[[{]")
            (let (forward-sexp-function) ; Use Lisp version.
              (condition-case nil
                  (progn
                    (forward-sexp)       ; Skip destructuring form.
                    (js--forward-syntactic-ws)
                    (if (and (/= (char-after) ?,) ; Regular array.
                             (looking-at "for"))
                        (match-beginning 0)))
                (scan-error
                 ;; Nothing to do here.
                 nil)))
          ;; To skip arbitrary expressions we need the parser,
          ;; so we'll just guess at it.
          (if (and (> end (point)) ; Not empty literal.
                   (re-search-forward "[^,]]* \\(for\\_>\\)" end t)
                   ;; Not inside comment or string literal.
                   (let ((status (parse-partial-sexp bracket (point))))
                     (and (= 1 (car status))
                          (not (nth 8 status)))))
              (match-beginning 1)))))))

(defun js--array-comp-indentation (bracket for-kwd)
  (if (js--same-line for-kwd)
      ;; First continuation line.
      (save-excursion
        (goto-char bracket)
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun js--maybe-goto-declaration-keyword-end (parse-status)
  "Helper function for `js--proper-indentation'.
Depending on the value of `js-indent-first-init', move
point to the end of a variable declaration keyword so that
indentation is aligned to that column."
  (cond
   ((eq js-indent-first-init t)
    (when (looking-at js--declaration-keyword-re)
      (goto-char (1+ (match-end 0)))))
   ((eq js-indent-first-init 'dynamic)
    (let ((bracket (nth 1 parse-status))
          declaration-keyword-end
          at-closing-bracket-p
          forward-sexp-function ; Use Lisp version.
          comma-p)
      (when (looking-at js--declaration-keyword-re)
        (setq declaration-keyword-end (match-end 0))
        (save-excursion
          (goto-char bracket)
          (setq at-closing-bracket-p
                (condition-case nil
                    (progn
                      (forward-sexp)
                      t)
                  (error nil)))
          (when at-closing-bracket-p
            (while (forward-comment 1))
            (setq comma-p (looking-at-p ","))))
        (when comma-p
          (goto-char (1+ declaration-keyword-end))))))))

(defconst js--line-terminating-arrow-re "=>\\s-*\\(/[/*]\\|$\\)"
  "Regexp matching the last \"=>\" (arrow) token on a line.
Whitespace and comments around the arrow are ignored.")

(defun js--broken-arrow-terminates-line-p ()
  "Helper function for `js--proper-indentation'.
Return non-nil if the last non-comment, non-whitespace token of the
current line is the \"=>\" token (of an arrow function)."
  (let ((from (point)))
    (end-of-line)
    (re-search-backward js--line-terminating-arrow-re from t)))

;; When indenting, we want to know if the line is…
;;
;;   - within a multiline JSXElement, or
;;   - within a string in a JSXBoundaryElement, or
;;   - within JSXText, or
;;   - within a JSXAttribute’s multiline JSXExpressionContainer.
;;
;; In these cases, special XML-like indentation rules for JSX apply.
;; If JS is nested within JSX, then indentation calculations may be
;; combined, such that JS indentation is “relative” to the JSX’s.
;;
;; Therefore, functions below provide such contextual information, and
;; `js--proper-indentation' may call itself once recursively in order
;; to finish calculating that “relative” JS+JSX indentation.

(defun js-jsx--context ()
  "Determine JSX context and move to enclosing JSX."
  (let ((pos (point))
        (parse-status (syntax-ppss))
        (enclosing-tag-pos (js-jsx--enclosing-tag-pos)))
    (when enclosing-tag-pos
      (if (< pos (nth 1 enclosing-tag-pos))
          (if (nth 3 parse-status)
              (list 'string (nth 8 parse-status))
            (list 'tag (nth 0 enclosing-tag-pos) (nth 1 enclosing-tag-pos)))
        (list 'text (nth 0 enclosing-tag-pos) (nth 2 enclosing-tag-pos))))))

(defun js-jsx--contextual-indentation (line context)
  "Calculate indentation column for LINE from CONTEXT.
The column calculation is based off of `sgml-calculate-indent'."
  (pcase (nth 0 context)

    ('string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (nth 1 context))
		 (zerop (forward-line -1))
		 (looking-at "[ \t]*$")))
     (if (> (point) (nth 1 context))
	 ;; Previous line is inside the string.
	 (current-indentation)
       (goto-char (nth 1 context))
       (1+ (current-column))))

    ('tag
     ;; Special JSX indentation rule: a “dangling” closing angle
     ;; bracket on its own line is indented at the same level as the
     ;; opening angle bracket of the JSXElement.  Otherwise, indent
     ;; JSXAttribute space like SGML.
     (if (and
          js-jsx-align->-with-<
          (progn
            (goto-char (nth 2 context))
            (and (= line (line-number-at-pos))
                 (looking-back "^\\s-*/?>" (line-beginning-position)))))
         (progn
           (goto-char (nth 1 context))
           (current-column))
       ;; Indent JSXAttribute space like SGML.
       (goto-char (nth 1 context))
       ;; Skip tag name:
       (skip-chars-forward " \t")
       (skip-chars-forward "^ \t\n")
       (skip-chars-forward " \t")
       (if (not (eolp))
	   (current-column)
         ;; This is the first attribute: indent.
         (goto-char (+ (nth 1 context) js-jsx-attribute-offset))
         (+ (current-column) (or js-jsx-indent-level js-indent-level)))))

    ('text
     ;; Indent to reflect nesting.
     (goto-char (nth 1 context))
     (+ (current-column)
        ;; The last line isn’t nested, but the rest are.
        (if (or (not (nth 2 context)) ; Unclosed.
                (< line (line-number-at-pos (nth 2 context))))
            (or js-jsx-indent-level js-indent-level)
          0)))

    ))

(defun js-jsx--enclosing-curly-pos ()
  "Return position of enclosing “{” in a “{/}” pair about point."
  (let ((parens (reverse (nth 9 (syntax-ppss)))) paren-pos curly-pos)
    (while
        (and
         (setq paren-pos (car parens))
         (not (when (= (char-after paren-pos) ?{)
                (setq curly-pos paren-pos)))
         (setq parens (cdr parens))))
    curly-pos))

(defun js-jsx--goto-outermost-enclosing-curly (limit)
  "Set point to enclosing “{” at or closest after LIMIT."
  (let (pos)
    (while
        (and
         (setq pos (js-jsx--enclosing-curly-pos))
         (if (>= pos limit) (goto-char pos))
         (> pos limit)))))

(defun js-jsx--expr-attribute-pos (start limit)
  "Look back from START to LIMIT for a JSXAttribute."
  (save-excursion
    (goto-char start) ; Skip the first curly.
    ;; Skip any remaining enclosing curlies until the JSXElement’s
    ;; beginning position; the last curly ought to be one of a
    ;; JSXExpressionContainer, which may refer to its JSXAttribute’s
    ;; beginning position (if it has one).
    (js-jsx--goto-outermost-enclosing-curly limit)
    (get-text-property (point) 'js-jsx-expr-attribute)))

(defvar js-jsx--indent-col nil
  "Baseline column for JS indentation within JSX.")

(defvar js-jsx--indent-attribute-line nil
  "Line relative to which indentation uses JSX as a baseline.")

(defun js-jsx--expr-indentation (parse-status pos col)
  "Indent using PARSE-STATUS; relative to POS, use base COL.
To indent a JSXExpressionContainer’s expression, calculate the JS
indentation, using JSX indentation as the base column when
indenting relative to the beginning line of the
JSXExpressionContainer’s JSXAttribute (if any)."
  (let* ((js-jsx--indent-col col)
         (js-jsx--indent-attribute-line
          (if pos (line-number-at-pos pos))))
    (js--proper-indentation parse-status)))

(defun js-jsx--indentation (parse-status)
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it is part
of a JSXElement expression spanning multiple lines; otherwise,
return nil."
  (let ((current-line (line-number-at-pos))
        (curly-pos (js-jsx--enclosing-curly-pos))
        nth-context context expr-p beg-line col
        forward-sexp-function) ; Use the Lisp version.
    ;; Find the immediate context for indentation information, but
    ;; keep going to determine that point is at the N+1th line of
    ;; multiline JSX.
    (save-excursion
      (while
          (and
           (setq nth-context (js-jsx--context))
           (progn
             (unless context
               (setq context nth-context)
               (setq expr-p (and curly-pos (< (point) curly-pos))))
             (setq beg-line (line-number-at-pos))
             (and
              (= beg-line current-line)
              (or (not curly-pos) (> (point) curly-pos)))))))
    ;; When on the second or later line of JSX, indent as JSX,
    ;; possibly switching back to JS indentation within
    ;; JSXExpressionContainers, possibly using the JSX as a base
    ;; column while switching back to JS indentation.
    (when (and context (> current-line beg-line))
      (save-excursion
        (setq col (js-jsx--contextual-indentation current-line context)))
      (if expr-p
          (js-jsx--expr-indentation
           parse-status (js-jsx--expr-attribute-pos curly-pos (nth 1 context)) col)
        col))))

(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((when (and js-jsx-syntax (not js-jsx--indent-col))
             (save-excursion (js-jsx--indentation parse-status))))
          ((and (eq (char-after) ?#)
                (save-excursion
                  (forward-char 1)
                  (looking-at-p cpp-font-lock-keywords-source-directives)))
           0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--chained-expression-p))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (or (not js-indent-align-list-continuation)
                     (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (save-excursion (forward-char) (js--broken-arrow-terminates-line-p)))
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (js--maybe-goto-declaration-keyword-end parse-status)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (+
                            (cond
                             ((and js-jsx--indent-attribute-line
                                   (eq js-jsx--indent-attribute-line
                                       (line-number-at-pos)))
                              js-jsx--indent-col)
                             (t
                              (current-column)))
                            (cond (same-indent-p 0)
                                  (continued-expr-p
                                   (+ (* 2 js-indent-level)
                                      js-expr-indent-offset))
                                  (t
                                   (+ js-indent-level
                                      (pcase (char-after (nth 1 parse-status))
                                        (?\( js-paren-indent-offset)
                                        (?\[ js-square-indent-offset)
                                        (?\{ js-curly-indent-offset))))))))
                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t (prog-first-column)))))

(defun js-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (line-beginning-position))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (js--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(defun js-jsx-indent-line ()
  "Indent the current line as JavaScript+JSX."
  (interactive)
  (let ((js-jsx-syntax t)) (js-indent-line)))

;;; Filling

(defvar js--filling-paragraph nil)

;; FIXME: Such redefinitions are bad style.  We should try and use some other
;; way to get the same result.
(defun js--fill-c-advice (js-fun)
  (lambda (orig-fun &rest args)
    (if js--filling-paragraph
        (funcall js-fun (car args))
      (apply orig-fun args))))

(advice-add 'c-forward-sws
            :around (js--fill-c-advice #'js--forward-syntactic-ws))
(advice-add 'c-backward-sws
            :around (js--fill-c-advice #'js--backward-syntactic-ws))
(advice-add 'c-beginning-of-macro
            :around (js--fill-c-advice #'js--beginning-of-macro))

(define-obsolete-function-alias 'js-c-fill-paragraph #'js-fill-paragraph "27.1")
(defun js-fill-paragraph (&optional justify)
  "Fill the paragraph for Javascript code."
  (interactive "*P")
  (let ((js--filling-paragraph t)
        (fill-paragraph-function #'c-fill-paragraph))
    (c-fill-paragraph justify)))

(defun js-do-auto-fill ()
  (let ((js--filling-paragraph t))
    (c-do-auto-fill)))

;;; Type database and Imenu

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of js--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially js--initial-pitem.
;;


(defun js--pitem-format (pitem)
  (let ((name (js--pitem-name pitem))
        (type (js--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun js--make-merged-item (item child name-parts)
  "Helper function for `js--splice-into-items'.
Return a new item that is the result of merging CHILD into
ITEM.  NAME-PARTS is a list of parts of the name of CHILD
that we haven't consumed yet."
  (js--debug "js--make-merged-item: {%s} into {%s}"
                   (js--pitem-format child)
                   (js--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (js--pitem-type item))
    (js--debug "js--make-merged-item: changing dest into class")
    (setq item (make-js--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (js--pitem-type child))
                          js--dummy-class-style
                  (js--pitem-type child))

                :name (js--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (js--debug "js--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (js--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (js--pitem-type child))
          (js--debug "js--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (js--debug "js--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun js--pitem-strname (pitem)
  "Last part of the name of PITEM, as a string or symbol."
  (let ((name (js--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun js--splice-into-items (items child name-parts)
  "Splice CHILD into the `js--pitem' ITEMS at NAME-PARTS.
If a class doesn't exist in the tree, create it.  Return
the new items list.  NAME-PARTS is a list of strings given
the broken-down class name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons)

    (js--debug "js--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'js--pitem-name items))

    (cl-assert (stringp top-name))
    (cl-assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (js--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (js--make-merged-item
                                             (car item-ptr) child
                                             name-parts)
                                            (cdr item-ptr)))

                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))

                       ;; ...and terminate the loop
                       nil)

                      (t
                       ;; Otherwise, copy the current cons and move onto the
                       ;; text. This is tricky; we keep track of the tail of
                       ;; the list that begins with new-items in
                       ;; last-new-item.
                       (setq new-cons (cons (car item-ptr) nil))
                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))
                       (setq last-new-item new-cons)

                       ;; Go to the next cell in items
                       (setq item-ptr (cdr item-ptr))))))

    (if item-ptr
        ;; Yay! We stopped because we found something, not because
        ;; we ran out of items to search. Just return the new
        ;; list.
        (progn
          (js--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (js--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-js--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (js--splice-into-items
                            nil child (cdr name-parts))
                 :type js--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun js--pitem-add-child (pitem child)
  "Copy `js--pitem' PITEM, and push CHILD onto its list of children."
  (cl-assert (integerp (js--pitem-h-begin child)))
  (cl-assert (if (consp (js--pitem-name child))
              (cl-loop for part in (js--pitem-name child)
                       always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (js--pitem-name child))
         (type (js--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (js--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `js--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (cl-assert (consp name))
            (js--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))

(defun js--maybe-make-marker (location)
  "Return a marker for LOCATION if `imenu-use-markers' is non-nil."
  (if imenu-use-markers
      (set-marker (make-marker) location)
    location))

(defun js--pitems-to-imenu (pitems unknown-ctr)
  "Convert PITEMS, a list of `js--pitem' structures, to imenu format."

  (let (imenu-items pitem pitem-type pitem-name subitems)

    (while (setq pitem (pop pitems))
      (setq pitem-type (js--pitem-type pitem))
      (setq pitem-name (js--pitem-strname pitem))
      (when (eq pitem-name t)
        (setq pitem-name (format "[unknown %s]"
                                 (cl-incf (car unknown-ctr)))))

      (cond
       ((memq pitem-type '(function macro))
        (cl-assert (integerp (js--pitem-h-begin pitem)))
        (push (cons pitem-name
                    (js--maybe-make-marker
                     (js--pitem-h-begin pitem)))
              imenu-items))

       ((consp pitem-type) ; class definition
        (setq subitems (js--pitems-to-imenu
                        (js--pitem-children pitem)
                        unknown-ctr))
        (cond (subitems
               (push (cons pitem-name subitems)
                     imenu-items))

              ((js--pitem-h-begin pitem)
               (cl-assert (integerp (js--pitem-h-begin pitem)))
               (setq subitems (list
                               (cons "[empty]"
                                     (js--maybe-make-marker
                                      (js--pitem-h-begin pitem)))))
               (push (cons pitem-name subitems)
                     imenu-items))))

       (t (error "Unknown item type: %S" pitem-type))))

    imenu-items))

(defun js--imenu-create-index ()
  "Return an imenu index for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (js--ensure-cache)
      (cl-assert (or (= (point-min) (point-max))
                  (eq js--last-parse-pos (point))))
      (when js--last-parse-pos
        (let ((state js--state-at-last-parse-pos)
              (unknown-ctr (cons -1 nil)))

          ;; Make sure everything is closed
          (while (cdr state)
            (setq state
                  (cons (js--pitem-add-child (cl-second state) (car state))
                        (cddr state))))

          (cl-assert (= (length state) 1))

          ;; Convert the new-finalized state into what imenu expects
          (js--pitems-to-imenu
           (car (js--pitem-children state))
           unknown-ctr))))))

;; Silence the compiler.
(defvar which-func-imenu-joiner-function)

(defun js--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

(defun js--imenu-to-flat (items prefix symbols)
  (cl-loop for item in items
           if (imenu--subalist-p item)
           do (js--imenu-to-flat
               (cdr item) (concat prefix (car item) ".")
               symbols)
           else
           do (let* ((name (concat prefix (car item)))
                     (name2 name)
                     (ctr 0))

                (while (gethash name2 symbols)
                  (setq name2 (format "%s<%d>" name (cl-incf ctr))))

                (puthash name2 (cdr item) symbols))))

(defun js--get-all-known-symbols ()
  "Return a hash table of all JavaScript symbols.
This searches all existing `js-mode' buffers.  Each key is the
name of a symbol (possibly disambiguated with <N>, where N > 1),
and each value is a marker giving the location of that symbol."
  (cl-loop with symbols = (make-hash-table :test 'equal)
           with imenu-use-markers = t
           for buffer being the buffers
           for imenu-index = (with-current-buffer buffer
                               (when (derived-mode-p 'js-mode)
                                 (js--imenu-create-index)))
           do (js--imenu-to-flat imenu-index "" symbols)
           finally return symbols))

(defvar js--symbol-history nil
  "History of entered JavaScript symbols.")

(defun js--read-symbol (symbols-table prompt &optional initial-input)
  "Helper function for `js-find-symbol'.
Read a symbol from SYMBOLS-TABLE, which is a hash table like the
one from `js--get-all-known-symbols', using prompt PROMPT and
initial input INITIAL-INPUT.  Return a cons of (SYMBOL-NAME
. LOCATION), where SYMBOL-NAME is a string and LOCATION is a
marker."
  (unless ido-mode
    (ido-mode 1)
    (ido-mode -1))

  (let ((choice (ido-completing-read
                 prompt
                 (cl-loop for key being the hash-keys of symbols-table
                          collect key)
                 nil t initial-input 'js--symbol-history)))
    (cons choice (gethash choice symbols-table))))

(defun js--guess-symbol-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (when (eq (char-before) ?.)
          (backward-char)
          (setf (car bounds) (point))))
      (buffer-substring (car bounds) (cdr bounds)))))

(declare-function xref-push-marker-stack "xref" (&optional m))

(defun js-find-symbol (&optional arg)
  "Read a JavaScript symbol and jump to it.
With a prefix argument, restrict symbols to those from the
current buffer.  Pushes a mark onto the tag ring just like
`find-tag'."
  (interactive "P")
  (require 'xref)
  (let (symbols marker)
    (if (not arg)
        (setq symbols (js--get-all-known-symbols))
      (setq symbols (make-hash-table :test 'equal))
      (js--imenu-to-flat (js--imenu-create-index)
                               "" symbols))

    (setq marker (cdr (js--read-symbol
                       symbols "Jump to: "
                       (js--guess-symbol-at-point))))

    (xref-push-marker-stack)
    (switch-to-buffer (marker-buffer marker))
    (push-mark)
    (goto-char marker)))

;;; Syntax extensions

(defvar js-syntactic-mode-name t
  "If non-nil, print enabled syntaxes in the mode name.")

(defun js--syntactic-mode-name-part ()
  "Return a string like “[JSX]” when `js-jsx-syntax' is enabled."
  (if js-syntactic-mode-name
      (let (syntaxes)
        (if js-jsx-syntax (push "JSX" syntaxes))
        (if syntaxes
            (concat "[" (mapconcat #'identity syntaxes ",") "]")
          ""))
    ""))

(defun js-use-syntactic-mode-name ()
  "Print enabled syntaxes if `js-syntactic-mode-name' is t.
Modes deriving from `js-mode' should call this to ensure that
their `mode-name' updates to show enabled syntax extensions."
  (when (stringp mode-name)
    (setq mode-name `(,mode-name (:eval (js--syntactic-mode-name-part))))))

(defun js-jsx-enable ()
  "Enable JSX in the current buffer."
  (interactive)
  (setq-local js-jsx-syntax t))

;; To make discovering and using syntax extensions features easier for
;; users (who might not read the docs), try to safely and
;; automatically enable syntax extensions based on heuristics.

(defvar js-jsx-regexps
  (list "\\_<\\(?:var\\|let\\|const\\|import\\)\\_>.*?React")
  "Case-sensitive regexps for detecting JSX in JavaScript buffers.
When `js-jsx-detect-syntax' is non-nil and any of these regexps
match text near the beginning of a JavaScript buffer,
`js-jsx-syntax' (which see) will be made buffer-local and set to
t.")

(defun js-jsx--detect-and-enable (&optional arbitrarily)
  "Detect if JSX is likely to be used, and enable it if so.
Might make `js-jsx-syntax' buffer-local and set it to t.  Matches
from the beginning of the buffer, unless optional arg ARBITRARILY
is non-nil.  Return t after enabling, nil otherwise."
  (when (or (and (buffer-file-name)
                 (string-match-p "\\.jsx\\'" (buffer-file-name)))
            (and js-jsx-detect-syntax
                 (save-excursion
                   (unless arbitrarily
                     (goto-char (point-min)))
                   (catch 'match
                     (mapc
                      (lambda (regexp)
                        (when (let (case-fold-search)
                                (re-search-forward regexp 4000 t))
                          (throw 'match t)))
                      js-jsx-regexps)
                     nil))))
    (js-jsx-enable)
    t))

(defun js-jsx--detect-after-change (beg end _len)
  "Detect if JSX is likely to be used after a change.
This function is intended for use in `after-change-functions'."
  (when (<= end 4000)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (save-restriction
        (narrow-to-region (point) end)
        (when (js-jsx--detect-and-enable 'arbitrarily)
          (remove-hook 'after-change-functions #'js-jsx--detect-after-change t))))))

;; Ensure all CC Mode "lang variables" are set to valid values.
;; js-mode, however, currently uses only those needed for filling.
(eval-and-compile
  (c-add-language 'js-mode 'java-mode))

(c-lang-defconst c-paragraph-start
  js-mode "\\(@[[:alpha:]]+\\>\\|$\\)")

;;; Tree sitter integration

(defun js--treesit-font-lock-compatibility-definition-feature ()
  "Font lock helper, to handle different releases of tree-sitter-javascript.
Check if a node type is available, then return the right font lock rules
for \"definition\" feature."
  (condition-case nil
      (progn (treesit-query-capture 'javascript '((function_expression) @cap))
             ;; Starting from version 0.20.2 of the grammar.
             '((function_expression
                name: (identifier) @font-lock-function-name-face)
               (variable_declarator
                name: (identifier) @font-lock-function-name-face
                value: [(function_expression) (arrow_function)])))
    (error
     ;; An older version of the grammar.
     '((function
        name: (identifier) @font-lock-function-name-face)
       (variable_declarator
        name: (identifier) @font-lock-function-name-face
        value: [(function) (arrow_function)])))))

(defun js-jsx--treesit-indent-compatibility-bb1f97b ()
  "Indent rules helper, to handle different releases of tree-sitter-javascript.
Check if a node type is available, then return the right indent rules."
  ;; handle commit bb1f97b
  (condition-case nil
      (progn (treesit-query-capture 'javascript '((jsx_fragment) @capture))
             `(((match "<" "jsx_fragment") parent 0)
               ((parent-is "jsx_fragment") parent js-indent-level)))
    (error
     `(((match "<" "jsx_text") parent 0)
       ((parent-is "jsx_text") parent js-indent-level)))))

(defvar js--treesit-indent-rules
  (let ((switch-case (rx "switch_" (or "case" "default"))))
    `((javascript
       ((parent-is "program") parent-bol 0)
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((and (parent-is "comment") c-ts-common-looking-at-star)
        c-ts-common-comment-start-after-first-star -1)
       ((parent-is "comment") prev-adaptive-prefix 0)
       ((parent-is "ternary_expression") parent-bol js-indent-level)
       ((parent-is "member_expression") parent-bol js-indent-level)
       ((node-is ,switch-case) parent-bol 0)
       ;; "{" on the newline.
       ((node-is "statement_block") parent-bol js-indent-level)
       ((parent-is "named_imports") parent-bol js-indent-level)
       ((parent-is "statement_block") parent-bol js-indent-level)
       ((parent-is "variable_declarator") parent-bol js-indent-level)
       ((parent-is "arguments") parent-bol js-indent-level)
       ((parent-is "array") parent-bol js-indent-level)
       ((parent-is "formal_parameters") parent-bol js-indent-level)
       ((parent-is "template_string") no-indent) ; Don't indent the string contents.
       ((parent-is "template_substitution") parent-bol js-indent-level)
       ((parent-is "object_pattern") parent-bol js-indent-level)
       ((parent-is "object") parent-bol js-indent-level)
       ((parent-is "pair") parent-bol js-indent-level)
       ((parent-is "arrow_function") parent-bol js-indent-level)
       ((parent-is "parenthesized_expression") parent-bol js-indent-level)
       ((parent-is "binary_expression") parent-bol js-indent-level)
       ((parent-is "class_body") parent-bol js-indent-level)
       ((parent-is ,switch-case) parent-bol js-indent-level)
       ((parent-is "statement_block") parent-bol js-indent-level)
       ((match "while" "do_statement") parent-bol 0)
       ((match "else" "if_statement") parent-bol 0)
       ((parent-is ,(rx (or (seq (or "if" "for" "for_in" "while" "do") "_statement")
                            "else_clause")))
        parent-bol js-indent-level)

       ;; JSX
       ,@(js-jsx--treesit-indent-compatibility-bb1f97b)
       ((node-is "jsx_closing_element") parent 0)
       ((match "jsx_element" "statement") parent js-indent-level)
       ((parent-is "jsx_element") parent js-indent-level)
       ((parent-is "jsx_text") parent-bol js-indent-level)
       ((parent-is "jsx_opening_element") parent js-indent-level)
       ((parent-is "jsx_expression") parent-bol js-indent-level)
       ((match "/" "jsx_self_closing_element") parent 0)
       ((parent-is "jsx_self_closing_element") parent js-indent-level)
       ;; FIXME(Theo): This no-node catch-all should be removed.  When is it needed?
       (no-node parent-bol 0)))))

(defvar js--treesit-keywords
  '("as" "async" "await" "break" "case" "catch" "class" "const" "continue"
    "debugger" "default" "delete" "do" "else" "export" "extends" "finally"
    "for" "from" "function" "get" "if" "import" "in" "instanceof" "let" "new"
    "of" "return" "set" "static" "switch" "switch" "target" "throw" "try"
    "typeof" "var" "void" "while" "with" "yield")
  "JavaScript keywords for tree-sitter font-locking.")

(defvar js--treesit-operators
  '("=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" ">>>=" "&=" "^="
    "|=" "&&=" "||=" "??=" "==" "!=" "===" "!==" ">" ">=" "<" "<=" "+"
    "-" "*" "/" "%" "++" "--" "**" "&" "|" "^" "~" "<<" ">>" ">>>"
    "&&" "||" "!")
  "JavaScript operators for tree-sitter font-locking.")

(defvar js--treesit-font-lock-settings
  (treesit-font-lock-rules

   :language 'javascript
   :feature 'comment
   '([(comment) (hash_bang_line)] @font-lock-comment-face)

   :language 'javascript
   :feature 'constant
   '(((identifier) @font-lock-constant-face
      (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))

     [(true) (false) (null)] @font-lock-constant-face)

   :language 'javascript
   :feature 'keyword
   `([,@js--treesit-keywords] @font-lock-keyword-face
     [(this) (super)] @font-lock-keyword-face)

   :language 'javascript
   :feature 'string
   '((regex pattern: (regex_pattern)) @font-lock-regexp-face
     (string) @font-lock-string-face)

   :language 'javascript
   :feature 'string-interpolation
   :override t
   '((template_string) @js--fontify-template-string
     (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

   :language 'javascript
   :feature 'definition
   `(,@(js--treesit-font-lock-compatibility-definition-feature)

     (class_declaration
      name: (identifier) @font-lock-type-face)

     (function_declaration
      name: (identifier) @font-lock-function-name-face)

     (method_definition
      name: (property_identifier) @font-lock-function-name-face)

     (formal_parameters
      [(identifier) @font-lock-variable-name-face
       (array_pattern (identifier) @font-lock-variable-name-face)
       (object_pattern (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (variable_declarator
      name: [(array_pattern (identifier) @font-lock-variable-name-face)
             (object_pattern
              (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

     ;; full module imports
     (import_clause (identifier) @font-lock-variable-name-face)
     ;; named imports with aliasing
     (import_clause (named_imports (import_specifier
                                    alias: (identifier) @font-lock-variable-name-face)))
     ;; named imports without aliasing
     (import_clause (named_imports (import_specifier
                                    !alias
                                    name: (identifier) @font-lock-variable-name-face)))

     ;; full namespace import (* as alias)
     (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

   :language 'javascript
   :feature 'assignment
   '((assignment_expression
      left: (_) @js--treesit-fontify-assignment-lhs))

   :language 'javascript
   :feature 'function
   '((call_expression
      function: [(identifier) @font-lock-function-call-face
                 (member_expression
                  property:
                  (property_identifier) @font-lock-function-call-face)]))

   :language 'javascript
   :feature 'jsx
   '((jsx_opening_element name: (_) @font-lock-function-call-face)
     (jsx_closing_element name: (_) @font-lock-function-call-face)
     (jsx_self_closing_element name: (_) @font-lock-function-call-face)
     (jsx_attribute (property_identifier) @font-lock-constant-face))

   :language 'javascript
   :feature 'property
   '(((property_identifier) @font-lock-property-use-face)
     (pair value: (identifier) @font-lock-variable-use-face)
     ((shorthand_property_identifier) @font-lock-property-use-face))

   :language 'javascript
   :feature 'number
   '((number) @font-lock-number-face
     ((identifier) @font-lock-number-face
      (:match "\\`\\(?:NaN\\|Infinity\\)\\'" @font-lock-number-face)))

   :language 'javascript
   :feature 'operator
   `([,@js--treesit-operators] @font-lock-operator-face
     (ternary_expression ["?" ":"] @font-lock-operator-face))

   :language 'javascript
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'javascript
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'javascript
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face))
  "Tree-sitter font-lock settings.")

(defun js--fontify-template-string (node override start end &rest _)
  "Fontify template string but not substitution inside it.
NODE is the template_string node.  START and END mark the region
to be fontified.

OVERRIDE is the override flag described in
`treesit-font-lock-rules'."
  ;; You would have thought that the children of the string node spans
  ;; the whole string.  No, the children of the template_string only
  ;; includes the starting "`", any template_substitution, and the
  ;; closing "`".  That's why we have to track BEG instead of just
  ;; fontifying each child.
  (let ((child (treesit-node-child node 0))
        (font-beg (treesit-node-start node)))
    (while child
      (let ((font-end (if (equal (treesit-node-type child)
                                 "template_substitution")
                          (treesit-node-start child)
                        (treesit-node-end child))))
        (setq font-beg (max start font-beg))
        (when (< font-beg end)
          (treesit-fontify-with-override
           font-beg font-end 'font-lock-string-face override start end)))
      (setq font-beg (treesit-node-end child)
            child (treesit-node-next-sibling child)))))

(defvar js--treesit-lhs-identifier-query
  (when (treesit-available-p)
    (treesit-query-compile 'javascript '((identifier) @id
                                         (property_identifier) @id
                                         (shorthand_property_identifier_pattern) @id)))
  "Query that captures identifier and query_identifier.")

(defun js--treesit-fontify-assignment-lhs (node override start end &rest _)
  "Fontify the lhs NODE of an assignment_expression.
For OVERRIDE, START, END, see `treesit-font-lock-rules'."
  (dolist (node (treesit-query-capture
                 node js--treesit-lhs-identifier-query nil nil t))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     (pcase (treesit-node-type node)
       ("identifier" 'font-lock-variable-use-face)
       ("property_identifier" 'font-lock-property-use-face)
       ("shorthand_property_identifier_pattern" 'font-lock-variable-use-face))
     override start end)))

(defun js--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (treesit-node-text
   (treesit-node-child-by-field-name
    (pcase (treesit-node-type node)
      ("lexical_declaration"
       (treesit-search-subtree node "variable_declarator" nil nil 1))
      ((or "function_declaration" "method_definition" "class_declaration")
       node))
    "name")
   t))

(defun js--treesit-valid-imenu-entry (node)
  "Return nil if NODE is a non-top-level \"lexical_declaration\"."
  (pcase (treesit-node-type node)
    ("lexical_declaration" (treesit-node-top-level node))
    (_ t)))

;;; Main Function

;;;###autoload
(define-derived-mode js-base-mode prog-mode "JavaScript"
  "Generic major mode for editing JavaScript.

This mode is intended to be inherited by concrete major modes.
Currently there are `js-mode' and `js-ts-mode'."
  :group 'js
  nil)

;;;###autoload
(define-derived-mode js-mode js-base-mode "JavaScript"
  "Major mode for editing JavaScript."
  :group 'js
  ;; Ensure all CC Mode "lang variables" are set to valid values.
  (c-init-language-vars js-mode)
  (setq-local indent-line-function #'js-indent-line)
  (setq-local beginning-of-defun-function #'js-beginning-of-defun)
  (setq-local end-of-defun-function #'js-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local font-lock-defaults
              (list js--font-lock-keywords nil nil nil nil
                    '(font-lock-syntactic-face-function
                      . js-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'js-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (add-hook 'syntax-propertize-extend-region-functions
            #'js--syntax-propertize-extend-region 'append 'local)
  (setq-local prettify-symbols-alist js--prettify-symbols-alist)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local which-func-imenu-joiner-function #'js--which-func-joiner)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  (setq-local fill-paragraph-function #'js-fill-paragraph)
  (setq-local normal-auto-fill-function #'js-do-auto-fill)

  ;; Parse cache
  (add-hook 'before-change-functions #'js--flush-caches t t)

  ;; Frameworks
  (js--update-quick-match-re)

  ;; Syntax extensions
  (unless (js-jsx--detect-and-enable)
    (add-hook 'after-change-functions #'js-jsx--detect-after-change nil t))
  (js-use-syntactic-mode-name)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (setq imenu-create-index-function #'js--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  (c-foreign-init-lit-pos-cache)
  (add-hook 'before-change-functions #'c-foreign-truncate-lit-pos-cache nil t)
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local comment-multi-line t)
  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
  (setq-local electric-layout-rules
	      '((?\; . after) (?\{ . after) (?\} . before)))

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    ;; While the full CC Mode style system is not yet in use, set the
    ;; pertinent style variables manually.
    (c-initialize-builtin-style)
    (let ((style (cc-choose-style-for-mode 'js-mode c-default-style)))
      (c-set-style style))
    (setq c-block-comment-prefix "* "
          c-comment-prefix-regexp "//+\\|\\**")
    (c-setup-paragraph-variables))

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expression literal and the problem
  ;; will mysteriously disappear.
  ;; FIXME: We should instead do this fontification lazily by adding
  ;; calls to syntax-propertize wherever it's really needed.
  ;;(syntax-propertize (point-max))
  )

;;;###autoload
(define-derived-mode js-ts-mode js-base-mode "JavaScript"
  "Major mode for editing JavaScript.

\\<js-ts-mode-map>"
  :group 'js
  :syntax-table js-mode-syntax-table
  (when (treesit-ready-p 'javascript)
    ;; Borrowed from `js-mode'.
    (setq-local prettify-symbols-alist js--prettify-symbols-alist)
    (setq-local parse-sexp-ignore-comments t)
    ;; Which-func.
    (setq-local which-func-imenu-joiner-function #'js--which-func-joiner)
    ;; Comment.
    (c-ts-common-comment-setup)
    (setq-local comment-multi-line t)
    ;; Electric-indent.
    (setq-local electric-indent-chars
                (append "{}():;,<>/" electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
    (setq-local electric-layout-rules
	        '((?\; . after) (?\{ . after) (?\} . before)))
    (setq-local syntax-propertize-function #'js-ts--syntax-propertize)

    ;; Tree-sitter setup.
    (treesit-parser-create 'javascript)
    ;; Indent.
    (setq-local treesit-simple-indent-rules js--treesit-indent-rules)
    ;; Navigation.
    (setq-local treesit-defun-prefer-top-level t)
    (setq-local treesit-defun-type-regexp
                (rx (or "class_declaration"
                        "method_definition"
                        "function_declaration"
                        "lexical_declaration")))
    (setq-local treesit-defun-name-function #'js--treesit-defun-name)
    ;; Fontification.
    (setq-local treesit-font-lock-settings js--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string)
                  ( assignment constant escape-sequence jsx number
                    pattern string-interpolation)
                  ( bracket delimiter function operator property)))
    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`function_declaration\\'" nil nil)
                  ("Variable" "\\`lexical_declaration\\'"
                   js--treesit-valid-imenu-entry nil)
                  ("Class" ,(rx bos (or "class_declaration"
                                        "method_definition")
                                eos)
                   nil nil)))
    (treesit-major-mode-setup)

    (add-to-list 'auto-mode-alist
                 '("\\(\\.js[mx]\\|\\.har\\)\\'" . js-ts-mode))))

(defvar js-ts--s-p-query
  (when (treesit-available-p)
    (treesit-query-compile 'javascript
                           '(((regex pattern: (regex_pattern) @regexp))
                             ((variable_declarator value: (jsx_element) @jsx))
                             ((assignment_expression right: (jsx_element) @jsx))
                             ((arguments (jsx_element) @jsx))
                             ((parenthesized_expression (jsx_element) @jsx))
                             ((return_statement (jsx_element) @jsx))))))

(defun js-ts--syntax-propertize (beg end)
  (let ((captures (treesit-query-capture 'javascript js-ts--s-p-query beg end)))
    (pcase-dolist (`(,name . ,node) captures)
      (let* ((ns (treesit-node-start node))
             (ne (treesit-node-end node))
             (syntax (pcase-exhaustive name
                       ('regexp
                        (cl-decf ns)
                        (cl-incf ne)
                        (string-to-syntax "\"/"))
                       ('jsx
                        (string-to-syntax "|")))))
        (put-text-property ns (1+ ns) 'syntax-table syntax)
        (put-text-property (1- ne) ne 'syntax-table syntax)))))

;;;###autoload
(define-derived-mode js-json-mode js-mode "JSON"
  (setq-local js-enabled-frameworks nil)
  ;; Speed up `syntax-ppss': JSON files can be big but can't hold
  ;; regexp matchers nor #! thingies (and `js-enabled-frameworks' is nil).
  (setq-local syntax-propertize-function #'ignore))

;; Since we made JSX support available and automatically-enabled in
;; the base `js-mode' (for ease of use), now `js-jsx-mode' simply
;; serves as one other interface to unconditionally enable JSX in
;; buffers, mostly for backwards-compatibility.
;;
;; Since it is probably more common for packages to integrate with
;; `js-mode' than with `js-jsx-mode', it is therefore probably
;; slightly better for users to use one of the many other methods for
;; enabling JSX syntax.  But using `js-jsx-mode' can’t be that bad
;; either, so we won’t bother users with an obsoletion warning.

;;;###autoload
(define-derived-mode js-jsx-mode js-mode "JavaScript"
  "Major mode for editing JavaScript+JSX.

Simply makes `js-jsx-syntax' buffer-local and sets it to t.

`js-mode' may detect and enable support for JSX automatically if
it appears to be used in a JavaScript file.  You could also
customize `js-jsx-regexps' to improve that detection; or, you
could set `js-jsx-syntax' to t in your init file, or in a
.dir-locals.el file, or using file variables; or, you could call
`js-jsx-enable' in `js-mode-hook'.  You may be better served by
one of the aforementioned options instead of using this mode."
  :group 'js
  (js-jsx-enable)
  (setq-local comment-region-function #'js-jsx--comment-region)
  (js-use-syntactic-mode-name))

(defun js-jsx--comment-region (beg end &optional arg)
  (if (or (js-jsx--context)
          (save-excursion
            (skip-chars-forward " \t")
            (js-jsx--looking-at-start-tag-p)))
      (let ((comment-start "{/* ")
            (comment-end " */}"))
        (comment-region-default beg end arg))
    (comment-region-default beg end arg)))

;;;###autoload (defalias 'javascript-mode 'js-mode)

(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'js-mode "// {{{" "// }}}" )))

;;;###autoload
(dolist (name (list "node" "nodejs" "gjs" "rhino"))
  (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'js-mode)))

(provide 'js)

;;; js.el ends here
