;;; ruby-ts-mode.el --- Major mode for editing Ruby files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Perry Smith <pedz@easesoftware.com>
;; Created: December 2022
;; Keywords: ruby languages tree-sitter
;; Version: 0.2

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

;;; Tree-sitter language versions
;;
;; ruby-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-ruby: v0.23.1
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:

;; This file defines ruby-ts-mode which is a major mode for editing
;; Ruby files that uses Tree Sitter to parse the language.  More
;; information about Tree Sitter can be found in the Elisp Info pages
;; as well as this website: https://tree-sitter.github.io/tree-sitter/

;; For this major mode to work, Emacs has to be compiled with
;; tree-sitter support, and the Ruby grammar has to be compiled and
;; put somewhere Emacs can find it.  See the docstring of
;; `treesit-extra-load-path'.

;; This mode doesn't associate itself with .rb files automatically.  To
;; use this mode by default, assuming you have the tree-sitter grammar
;; available, do one of the following:
;;
;; - Add the following to your init file:
;;
;;    (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
;;
;; - Customize 'auto-mode-alist' to turn ruby-ts-mode automatically.
;;   For example:
;;
;;    (add-to-list 'auto-mode-alist
;;                 (cons (concat "\\(?:\\.\\(?:"
;;                               "rbw?\\|ru\\|rake\\|thor\\|axlsx"
;;                               "\\|jbuilder\\|rabl\\|gemspec\\|podspec"
;;                               "\\)"
;;                               "\\|/"
;;                               "\\(?:Gem\\|Rake\\|Cap\\|Thor"
;;                               "\\|Puppet\\|Berks\\|Brew\\|Fast"
;;                               "\\|Vagrant\\|Guard\\|Pod\\)file"
;;                               "\\)\\'")
;;                       'ruby-ts-mode))
;;
;;   will turn on the ruby-ts-mode for Ruby source files.
;;
;; - If you have the Ruby grammar installed, customize
;;   'treesit-enabled-modes' and add 'ruby-ts-mode' to it.
;;
;; You can also turn on this mode manually in a buffer.

;; Tree Sitter brings a lot of power and versitility which can be
;; broken into these features.

;; * Font Lock

;; The ability to color the source code is not new but what is new is
;; the versatility to enable and disable particular font lock rules.
;; I suggest reviewing variable treesit-font-lock-level and function
;; treesit-font-lock-recompute-features to get a better understanding
;; of the following.

;; Currently tree treesit-font-lock-feature-list is set with the
;; following levels:
;;   1: comment method-definition parameter-definition
;;   2: keyword regexp string type
;;   3: builtin-variable builtin-constant builtin-function
;;      delimiter escape-sequence
;;      constant global instance
;;      interpolation literal symbol assignment
;;   4: bracket error function operator punctuation

;; Thus if treesit-font-lock-level is set to level 3 which is its
;; default, all the features listed in levels 1 through 3 above will
;; be enabled.  i.e. those features will font lock or colorize the
;; code accordingly.  Individual features can be added and removed via
;; treesit-font-lock-recompute-features.

;; describe-face can be used to view how a face looks.

;; * Indent

;; ruby-ts-mode tries to adhere to the indentation related user
;; options from ruby-mode, such as ruby-indent-level,
;; ruby-indent-tabs-mode, and so on.
;;
;; Type 'M-x customize-group RET ruby RET' to see the options.

;; * IMenu
;; * Navigation
;; * Which-func

;;; Code:

(require 'treesit)
(require 'ruby-mode)
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(ruby "https://github.com/tree-sitter/tree-sitter-ruby"
        :commit "71bd32fb7607035768799732addba884a37a6210")
 t)

(defgroup ruby-ts nil
  "Major mode for editing Ruby code."
  :prefix "ruby-ts-"
  :group 'languages)

(defvar ruby-ts--operators
  '("+" "-" "*" "/" "%" "**"
    "==" "!=" ">" "<" ">=" "<=" "<=>" "==="
    "=" "+=" "-=" "*=" "/=" "%=" "**="
    "&" "|" "^" "~" "<<" ">>"
    "!" "&&" "and" "not" "or" "||"
    "?" ":"
    ".." "..."
    "defined?"
    "." "::")
  "Ruby operators for tree-sitter font-locking.")

(defvar ruby-ts--delimiters '("," ";")
  "Ruby's punctuation characters.")

(defvar ruby-ts--predefined-constants
  (rx string-start
      (or "ARGF" "ARGV" "DATA" "ENV" "RUBY_COPYRIGHT"
          "RUBY_DESCRIPTION" "RUBY_ENGINE" "RUBY_ENGINE_VERSION"
          "RUBY_PATCHLEVEL" "RUBY_PLATFORM" "RUBY_RELEASE_DATE"
          "RUBY_REVISION" "RUBY_VERSION" "STDERR" "STDIN" "STDOUT"
          "TOPLEVEL_BINDING")
      string-end)
  "Ruby predefined global constants.")

(defvar ruby-ts--predefined-variables
  (rx string-start
      (or "$!" "$@" "$~" "$&" "$`" "$'" "$+" "$=" "$/" "$\\" "$," "$;"
          "$." "$<" "$>" "$_" "$*" "$$" "$?" "$:" "$LOAD_PATH"
          "$LOADED_FEATURES" "$DEBUG" "$FILENAME" "$stderr" "$stdin"
          "$stdout" "$VERBOSE" "$-a" "$-i" "$-l" "$-p"
          "$0" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9")
      string-end)
  "Ruby predefined global variables.")

(defvar ruby-ts--builtin-methods
  (format "\\`%s\\'" (regexp-opt (append ruby-builtin-methods-no-reqs
                                         ruby-builtin-methods-with-reqs)))
  "Ruby built-in methods.")

(defconst ruby-ts--class-or-module-regex
  (rx string-start
      (or "class" "module" "singleton_class")
      string-end)
  "Regular expression that matches a class or module's node type.")

(defconst ruby-ts--method-regex
  (rx string-start
      (or "method" "singleton_method")
      string-end)
  "Regular expression matching methods and singleton methods.")

(defconst ruby-ts--statement-container-regexp
  (rx string-start
      (or "program"
          "block_body"
          "begin_block"
          "end_block"
          "do"
          "else"
          "then"
          "ensure"
          "body_statement"
          "interpolation")
      string-end)
  "Regular expression of the nodes that can contain statements.")

(defun ruby-ts--lineno (node)
  "Return line number of NODE's start."
  (line-number-at-pos (treesit-node-start node)))

;; doc/keywords.rdoc in the Ruby git repository considers these to be
;; reserved keywords.  If these keywords are added to the list, it
;; causes the font-lock to stop working.
;;
;; "__ENCODING__" "__FILE__" "__LINE__" "false" "self" "super" "true"
;;
;; "nil" (which does not exhibit this issue) is also considered a
;; keyword but I removed it and added it as a constant.
;;
(defvar ruby-ts--keywords
  '("BEGIN" "END" "alias" "and" "begin" "break" "case" "class"
    "def" "defined?" "do" "else" "elsif" "end" "ensure" "for"
    "if" "in" "module" "next" "not" "or" "redo" "rescue"
    "retry" "return" "then" "undef" "unless" "until" "when"
    "while" "yield")
  "Ruby keywords for tree-sitter font-locking.")

(defun ruby-ts--comment-font-lock (node override start end &rest _)
  "Apply font lock to comment NODE within START and END.
Applies `font-lock-comment-delimiter-face' and
`font-lock-comment-face'.  See `treesit-fontify-with-override' for
values of OVERRIDE."
  ;; Empirically it appears as if (treesit-node-start node) will be
  ;; where the # character is at and (treesit-node-end node) will be
  ;; the end of the line
  (let* ((node-start (treesit-node-start node))
         (plus-1 (1+ node-start))
         (node-end (treesit-node-end node))
         (text (treesit-node-text node t)))
    (if (and (>= node-start start)
             (<= plus-1 end)
             (string-match-p "\\`#" text))
        (treesit-fontify-with-override node-start plus-1
                                       'font-lock-comment-delimiter-face override))
    (treesit-fontify-with-override (max plus-1 start) (min node-end end)
                                   'font-lock-comment-face override)))

(defun ruby-ts--font-lock-settings (language)
  "Tree-sitter font-lock settings for Ruby."
  (treesit-font-lock-rules
   :language language
   :feature 'comment
   '((comment) @ruby-ts--comment-font-lock)

   :language language
   :feature 'builtin-variable
   `(((global_variable) @var (:match ,ruby-ts--predefined-variables @var)) @font-lock-builtin-face)

   :language language
   :feature 'builtin-constant
   `(((constant) @var (:match ,ruby-ts--predefined-constants @var)) @font-lock-builtin-face)

   :language language
   :feature 'keyword
   `([,@ruby-ts--keywords] @font-lock-keyword-face
     (self) @font-lock-keyword-face
     (super) @font-lock-keyword-face)

   :language language
   :feature 'constant
   '((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (nil) @font-lock-constant-face)

   ;; Before 'operator so (unary) works.
   :language language
   :feature 'literal
   '((unary ["+" "-"] [(integer) (rational) (float) (complex)]) @font-lock-number-face
     (integer) @font-lock-number-face
     (float) @font-lock-number-face
     (complex) @font-lock-number-face
     (rational) @font-lock-number-face)

   ;; Also before 'operator because % and / are operators
   :language language
   :feature 'regexp
   '((regex "/" @font-lock-regexp-face)
     (regex _ (string_content) @font-lock-regexp-face))

   :language language
   :feature 'operator
   `("!" @font-lock-negation-char-face
     [,@ruby-ts--operators] @font-lock-operator-face)

   ;; TODO: Consider using a different face for string delimiters.
   ;; font-lock-delimiter-face is not a good choice, though, because it
   ;; looks like 'default' in the default theme, and its documented purpose
   ;; is characters like commas, semicolons, etc.
   :language language
   :feature 'string
   '((delimited_symbol [ ":\"" "\"" ] @font-lock-string-face)
     (string "\"" @font-lock-string-face)
     (string_array ["%w(" ")"] @font-lock-string-face)
     (subshell "`" @font-lock-string-face)
     (symbol_array ["%i(" ")"] @font-lock-constant-face))

   :language language
   :feature 'string
   '([(string_content)
      (heredoc_beginning)
      (heredoc_content)
      (heredoc_end)]
     @font-lock-string-face)

   :language language
   :feature 'interpolation
   '((interpolation "#{" @font-lock-misc-punctuation-face)
     (interpolation "}" @font-lock-misc-punctuation-face))

   :language language
   :feature 'type
   '((constant) @font-lock-type-face)

   :language language
   :feature 'global
   '((global_variable) @font-lock-variable-use-face)

   :language language
   :feature 'instance
   '((instance_variable) @font-lock-variable-use-face)

   :language language
   :feature 'method-definition
   '((method
      name: (identifier) @font-lock-function-name-face)
     (singleton_method
      name: (identifier) @font-lock-function-name-face)
     (method
      name: (setter) @font-lock-function-name-face))

   :language language
   :feature 'parameter-definition
   '((method_parameters
      (identifier) @font-lock-variable-name-face)
     (block_parameters
      (identifier) @font-lock-variable-name-face)
     (optional_parameter
      name: (identifier) @font-lock-variable-name-face)
     (splat_parameter
      name: (identifier) @font-lock-variable-name-face)
     (hash_splat_parameter
      name: (identifier) @font-lock-variable-name-face)
     (block_parameter
      name: (identifier) @font-lock-variable-name-face)
     (destructured_parameter
      (identifier) @font-lock-variable-name-face)
     (lambda_parameters
      (identifier) @font-lock-variable-name-face)
     (exception_variable
      (identifier) @font-lock-variable-name-face)
     (array_pattern
      (identifier) @font-lock-variable-name-face)
     (keyword_pattern
      value: (identifier) @font-lock-variable-name-face)
     (keyword_pattern
      key: (hash_key_symbol) @font-lock-variable-name-face
      !value)
     (as_pattern
      name: (identifier) @font-lock-variable-name-face)
     (in_clause
      pattern: (identifier) @font-lock-variable-name-face))

   :language language
   :feature 'builtin-function
   `((((identifier) @font-lock-builtin-face)
      (:match ,ruby-ts--builtin-methods @font-lock-builtin-face)))

   ;; Yuan recommends also putting method definitions into the
   ;; 'function' category (thus keeping it in both).  I've opted to
   ;; just use separate categories for them -- dgutov.
   :language language
   :feature 'function
   '((call
      method: (identifier) @font-lock-function-call-face))

   :language language
   :feature 'assignment
   '((assignment
      left: (identifier) @font-lock-variable-name-face)
     (assignment
      left: (left_assignment_list (identifier) @font-lock-variable-name-face))
     (operator_assignment
      left: (identifier) @font-lock-variable-name-face))

   :language language
   :feature 'symbol
   '((bare_symbol) @font-lock-constant-face
     (delimited_symbol (string_content) @font-lock-constant-face)
     (hash_key_symbol) @font-lock-constant-face
     (simple_symbol) @font-lock-constant-face)

   :language language
   :feature 'error
   '((ERROR) @font-lock-warning-face)

   :feature 'escape-sequence
   :language language
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language language
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language language
   :feature 'punctuation
   `(([,@ruby-ts--delimiters] @font-lock-delimiter-face))))

(defun ruby-ts--first-non-comment-child (node)
  "Return the first named child of NODE that is not a comment."
  (let ((child (treesit-node-child node 0 t)))
    (while (and child
                (equal "comment" (treesit-node-type child)))
      (setq child (treesit-node-next-sibling child t)))
    child))

;;
;; These routines would be better added to treesit.el  They are
;; intended to be used with indent rules
;;
;; I think this is over simplified but basically
;; treesit--simple-indent-eval calls the result with node, parent, and
;; bol. Thus all of these functions return a lambda that accepts three
;; arguments.  Somewhere something explains that &rest should always
;; be used in case extra arguments are added in the future.
;;

(defun ruby-ts--type-pred (regexp)
  "Return predicate taking a node returning non-nil if REGEXP matches type of node."
  (lambda (node)
    (string-match-p regexp (treesit-node-type node))))

(defun ruby-ts--parent-node (_n parent &rest _)
  "Return the PARENT node matching ident rule."
  parent)

(defun ruby-ts--align-keywords (pred)
  "Return either start or bol of PRED.
PRED should specify a node that is listed in
`ruby-alignable-keywords'.  If PRED is listed in user option
`ruby-align-to-stmt-keywords', then return the BOL of PRED.
Otherwise return start of PRED."
  (lambda (node parent bol &rest rest)
    (let* ((pred-node (funcall pred node parent bol rest))
           (temp (treesit-node-start pred-node))
           (type (treesit-node-type pred-node))
           (bol (ruby-smie--indent-to-stmt-p
                 (if (equal type "method")
                     "def"
                   type))))
      (when temp
        (if bol
            (save-excursion
              (goto-char temp)
              (back-to-indentation)
              (point))
          temp)))))

(defun ruby-ts--bol (pred)
  "Return bol of PRED.
PRED should take (node parent bol &rest rest) and return a node.
Returns bol of the current line if PRED returns nil."
  (lambda (node parent bol &rest rest)
    (save-excursion
      (let ((temp (treesit-node-start (funcall pred node parent bol rest))))
        (if temp
            (goto-char temp))
        (back-to-indentation)
        (point)))))

(defun ruby-ts--grand-parent-node (_n parent &rest _)
  "Return parent of PARENT node."
  (treesit-node-parent parent))

(defun ruby-ts--align-chain-p (&rest _)
  "Return value of `ruby-align-chained-calls'."
  ruby-align-chained-calls)

(defun ruby-ts--parenless-call-arguments-indent-p (&rest _)
  "Return value of `ruby-parenless-call-arguments-indent'."
  ruby-parenless-call-arguments-indent)

(defun ruby-ts--align-chain (_n parent &rest _)
  "Align chained method call.
Align NODE which will be the dot (.) to the dot of the
first (outermost) call in the chain.  See
`ruby-align-chained-calls' for details.  PARENT will be the
\"call\" node.  Called only when `ruby-align-chained-calls' is
non-nil."
  (let* (first-call )
    (while (and parent
                (setq first-call (treesit-node-parent parent))
                (equal "call" (treesit-node-type first-call)))
      (setq parent first-call))
    (treesit-node-start (treesit-search-subtree parent "\\." nil t))))

(defun ruby-ts--same-line-args-p (_n parent &rest _)
  "Return non-nil when first argument is on the same line as the method.
PARENT will be argument_list.  NODE can be the close paren."
  (let* ((method (treesit-node-parent parent))
         (first-param (ruby-ts--first-non-comment-child parent)))
    (= (ruby-ts--lineno method) (ruby-ts--lineno first-param))))

(defun ruby-ts--same-line-params-p (_n parent &rest _)
  "Return non-nil when first parameter is on the same line as the method.
PARENT will be method_parameters.  NODE can be the close paren."
  (let* ((method (treesit-node-parent parent))
         (first-param (ruby-ts--first-non-comment-child parent)))
    (= (ruby-ts--lineno method) (ruby-ts--lineno first-param))))

(defun ruby-ts--param-indent (_n parent &rest _)
  "Indent parameters that start on next line.
Given: NODE is the parameter.  PARENT is
method_parameters.  `ruby-ts--same-line-params-p' is nil.
Indent according to `ruby-method-params-indent'.

If `ruby-method-params-indent' is 0
def foo(
  param1,
  param2
)

Params start on next line, `ruby-method-params-indent' is t
def foo(
      param1,
      param2
    )"
  (let ((method (treesit-node-parent parent)))
    (if (eq t ruby-method-params-indent)
        ;; For methods, the "name" is the name of the method but for
        ;; singleton methods, we need to find "object"
        (let* ((singleton (equal "singleton_method" (treesit-node-type method)))
               (name-node (treesit-node-child-by-field-name
                           method
                           (if singleton "object" "name"))))
          ;; (message "name-node: %S" name-node)
          (treesit-node-start name-node))
      ;; Small Danger: if the method name plus the parent is less than
      ;; `ruby-method-params-indent', then the addition will put the
      ;; result on the next line and indented incorrectly.  There are
      ;; plausible ways to fix this but the probability seems rather
      ;; remote.
      (+ (treesit-node-start method) (or ruby-method-params-indent 0)))))

(defun ruby-ts--true (&rest _)
  "I have no idea why I can't just put t but I can put 0."
  t)

(defun ruby-ts--same-line-hash-array-p (_n parent &rest _)
  "Return non-nil if first element and open brace are on the same line.
NODE is the element or closing brace or bracket.  PARENT is the
array or hash."
  (let* ((open-brace (treesit-node-child parent 0 nil))
         (first-child (ruby-ts--first-non-comment-child parent)))
    (= (ruby-ts--lineno open-brace) (ruby-ts--lineno first-child))))

(defun ruby-ts--statement-ancestor (node &rest _)
  "Return the statement ancestor of NODE if any.
A statement is defined as a child of a statement container where
a statement container is a node that matches
`ruby-ts--statement-container-regexp'."
  (let* ((statement node)
         (parent (treesit-node-parent statement)))
    (while (and parent
                statement
                (not (string-match-p ruby-ts--statement-container-regexp
                                     (treesit-node-type parent))))
      (setq statement parent
            parent (treesit-node-parent parent)))
    statement))

;;
;; end of functions that can be used for queries
;;

(defun ruby-ts--indent-rules ()
  "Indent rules supported by `ruby-ts-mode'."
  (let ((common
         `(
           ;; Slam all top level nodes to the left margin
           ((parent-is "program") column-0 0)

           ;; Do not indent here docs or the end.  Not sure why it
           ;; takes the grand-parent but ok fine.
           ((n-p-gp nil nil "heredoc_body") no-indent 0)
           ((parent-is "heredoc_body") no-indent 0)
           ((node-is "heredoc_body") no-indent 0)
           ;; Do not indent multiline regexp
           ((n-p-gp nil nil "regex") no-indent 0)
           ((parent-is "regex") no-indent 0)

           ;; Incomplete buffer state, better not reindent (bug#61017).
           ((and (parent-is "ERROR")
                 (or (node-is ,ruby-ts--class-or-module-regex)
                     (node-is "\\`\\(?:def\\|identifier\\)\\'")))
            no-indent 0)

           ;; if then else elseif notes:
           ;;
           ;;   1. The "then" starts at the end of the line that ends
           ;;      the if condition which can be on a different line
           ;;      from the "if".
           ;;
           ;;   2. If there is an "elsif", it is a sibling to the then
           ;;      BUT the "else" that follows is now a child of the
           ;;      "elsif".
           ;;
           ;;   3. The statements within each of these are direct
           ;;      children.  There is no intermediate construct such
           ;;      as a block_statement.
           ;;
           ;; I'm using very restrictive patterns hoping to reduce rules
           ;; triggering unintentionally.
           ((match "else" "if\\|unless")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((match "elsif" "if")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((match "end" "if\\|unless")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((n-p-gp nil "then\\|else\\|elsif" "if\\|unless")
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) ruby-indent-level)

           ;; case expression: when, in_clause, and else are all
           ;; children of case.  when and in_clause have pattern and
           ;; body as fields.  body has "then" and then the statements.
           ;; i.e. the statements are not children of when but then.
           ;; But for the statements are children of else.
           ((match "when" "case")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((match "in_clause" "case")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((match "else" "case")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((match "end" "case")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((n-p-gp nil "then" "when") grand-parent ruby-indent-level)
           ((n-p-gp nil "then" "in_clause") grand-parent ruby-indent-level)
           ((n-p-gp nil "else" "case") parent ruby-indent-level)

           ;; The beauty of inconsistency :-)
           ;; while / until have only "do" as a child.  The "end" is a
           ;; child of "do".
           ((n-p-gp "end" "do" "while\\|until")
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) 0)
           ((n-p-gp nil "do" "while\\|until")
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) ruby-indent-level)
           ;; begin can have rescue, ensure, else, and end.
           ;; statements are a child of begin.  rescue, ensure, else,
           ;; and end are also children of begin.  rescue has a then
           ;; as a child thus statements will be grand children of
           ;; rescue.
           ((n-p-gp nil "then" "rescue")
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) ruby-indent-level)
           ((n-p-gp nil "ensure\\|else" "begin")
            (ruby-ts--align-keywords ruby-ts--parent-node) ruby-indent-level)
           ((match "rescue\\|ensure\\|else\\|end" "begin")
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((parent-is "begin")           ;last
            (ruby-ts--align-keywords ruby-ts--parent-node) ruby-indent-level)

           ;; for ... I don't think I have ever used a for loop in
           ;; Ruby.  The "in" (not an in_clause) and "do" are
           ;; children.  The statements are children of the "do".
           ;; And, of course, the "end" is a child of the "do".
           ((n-p-gp "end" "do" "for")
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) 0)
           ((n-p-gp nil "do" "for")
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) ruby-indent-level)

           ;; method has a "body_statement" and the "end" as children.
           ;; The body_statement can have rescue, ensure, and else as
           ;; well as statements.  Note that the first statement of a
           ;; body_statement hits the node as "body_statement" and not
           ;; as the assignment, etc.
           ((match "end" ,ruby-ts--method-regex)
            (ruby-ts--align-keywords ruby-ts--parent-node) 0)
           ((n-p-gp "\\`\\(rescue\\|ensure\\|else\\)\\'" "body_statement" ,ruby-ts--method-regex)
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) 0)
           ((n-p-gp nil "rescue\\|ensure\\|else" "body_statement") parent ruby-indent-level)
           ((match "body_statement" ,ruby-ts--method-regex) ;first statement
            (ruby-ts--align-keywords ruby-ts--parent-node) ruby-indent-level)
           ((n-p-gp nil "body_statement" ,ruby-ts--method-regex) ;other statements
            (ruby-ts--align-keywords ruby-ts--grand-parent-node) ruby-indent-level)

           ;; Quirk of the ruby parser: these "alignable" nodes don't
           ;; have the "container" child node when there are no
           ;; statements inside. Thus we have to have a separate rule
           ;; for the "empty if/unless/case/def" situation.
           ((match "\\`\\'" "\\`\\(?:if\\|unless\\|case\\|method\\)\\'")
            (ruby-ts--align-keywords ruby-ts--parent-node) ruby-indent-level)

           ;; Chained calls:
           ;; if `ruby-align-chained-calls' is true, the first query
           ;; matches and the node is aligned under the first dot (.);
           ;; else the second query aligns
           ;; `ruby-indent-level' spaces in from the parent.
           ((and ruby-ts--align-chain-p (match "\\." "call")) ruby-ts--align-chain 0)
           ;; Obery ruby-method-call-indent, whether the dot is on
           ;; this line or the previous line.
           ((and (not ruby-ts--method-call-indent-p)
                 (or
                  (match "\\." "call")
                  (query "(call \".\" (identifier) @indent)")))
            (ruby-ts--bol ruby-ts--statement-ancestor) ruby-indent-level)
           ((match "\\." "call") parent ruby-indent-level)

           ;; method parameters -- four styles:
           ;; 1) With paren, first arg on same line:
           ((and (query "(method_parameters \"(\" _ @indent)")
                 ruby-ts--same-line-params-p
                 (node-is ")"))
            first-sibling 0)
           ((and (query "(method_parameters \"(\" _ @indent)")
                 ruby-ts--same-line-params-p)
            first-sibling 1)
           ;; ;; 2) With paren, first arg on next line, ruby-method-params-indent eq t
           ;; ;; 3) With paren, first arg on next line, ruby-method-params-indent neq t
           ((and (query "(method_parameters \"(\" _ @indent)") (node-is ")")) ruby-ts--param-indent 0)
           ((query "(method_parameters \"(\" _ @indent)") ruby-ts--param-indent ruby-indent-level)
           ;; 4) No paren:
           ((parent-is "method_parameters") first-sibling 0)

           ;; Argument lists:
           ;; 1) With paren, 1st arg on same line
           ((and (query "(argument_list \"(\" _ @indent)")
                 ruby-ts--same-line-args-p
                 (node-is ")"))
            first-sibling 0)
           ((and (query "(argument_list \"(\" _ @indent)")
                 ruby-ts--same-line-args-p)
            first-sibling 1)
           ;; 2) With paren, 1st arg on next line
           ((and (query "(argument_list \"(\" _ @indent)")
                 (node-is ")"))
            ruby-ts--parent-call-or-bol 0)
           ((or (query "(argument_list \"(\" _ @indent)")
                ;; No arguments yet; NODE is nil in that case.
                (match "\\`\\'" "argument_list"))
            ruby-ts--parent-call-or-bol ruby-indent-level)
           ;; 3) No paren, ruby-parenless-call-arguments-indent is t
           ((and ruby-ts--parenless-call-arguments-indent-p (parent-is "argument_list"))
            first-sibling 0)
           ;; 4) No paren, ruby-parenless-call-arguments-indent is nil
           ((parent-is "argument_list")
            (ruby-ts--bol ruby-ts--statement-ancestor) ruby-indent-level)

           ;; Old... probably too simple
           ((parent-is "block_parameters") first-sibling 1)

           ((and (not ruby-ts--after-op-indent-p)
                 (parent-is "binary\\|conditional"))
            (ruby-ts--bol ruby-ts--statement-ancestor) ruby-indent-level)

           ((parent-is "binary")
            ruby-ts--binary-indent-anchor 0)

           ((parent-is "conditional") parent ruby-indent-level)

           ;; ruby-mode does not touch these...
           ((match "bare_string" "string_array") no-indent 0)

           ;; hash and array.  Note that the first sibling is the "{"
           ;; or "[".  There is a special case where the hash is an
           ;; argument to a method.  These need to be processed first.

           ((and ruby-ts--same-line-hash-array-p (match "}" "hash"))
            first-sibling 0)
           ((and ruby-ts--same-line-hash-array-p (parent-is "hash"))
            (nth-sibling 0 ruby-ts--true) 0)
           ((and ruby-ts--same-line-hash-array-p (match "]" "array"))
            first-sibling 0)
           ((and ruby-ts--same-line-hash-array-p (parent-is "array"))
            (nth-sibling 0 ruby-ts--true) 0)

           ((match "}" "hash")  ruby-ts--parent-call-or-bol 0)
           ((parent-is "hash")  ruby-ts--parent-call-or-bol ruby-indent-level)
           ((match "]" "^array") ruby-ts--parent-call-or-bol 0)
           ((parent-is "^array") ruby-ts--parent-call-or-bol ruby-indent-level)
           ((match ")" "string_array") ruby-ts--parent-call-or-bol 0)

           ((parent-is "pair") ruby-ts--parent-call-or-bol 0)

           ((match ")" "parenthesized_statements") parent-bol 0)
           ((parent-is "parenthesized_statements") parent-bol ruby-indent-level)

           ;; If the previous method isn't finished yet, this will get
           ;; the next method indented properly.
           ((n-p-gp ,ruby-ts--method-regex "body_statement" ,ruby-ts--class-or-module-regex)
            (ruby-ts--bol ruby-ts--grand-parent-node) ruby-indent-level)

           ;; Match the end of a class / module
           ((match "end" ,ruby-ts--class-or-module-regex) parent 0)

           ;; A "do_block" has a "body_statement" child which has the
           ;; statements as children within it.  The problem is that
           ;; the first statement starts at the same point as the
           ;; body_statement and so treesit-simple-indent is called
           ;; with node set to body_statement on the first statement
           ;; but with node set to the statement and parent set to
           ;; body_statement for all others. ... Fine.  Be that way.
           ;; Ditto for "block" and "block_body"
           ((node-is "body_statement")
            (ruby-ts--block-indent-anchor ruby-ts--parent-node)
            ruby-indent-level)
           ((parent-is "body_statement")
            (ruby-ts--block-indent-anchor ruby-ts--grand-parent-node)
            ruby-indent-level)
           ((match "end" "do_block") (ruby-ts--block-indent-anchor ruby-ts--parent-node) 0)
           ((n-p-gp "block_body" "block" nil)
            (ruby-ts--block-indent-anchor ruby-ts--parent-node)
            ruby-indent-level)
           ((n-p-gp nil "block_body" "block")
            (ruby-ts--block-indent-anchor ruby-ts--grand-parent-node)
            ruby-indent-level)
           ((match "}" "block") (ruby-ts--block-indent-anchor ruby-ts--parent-node) 0)

           ;; Chained strings
           ((match "string" "chained_string") first-sibling 0)

           ;; Try and indent two spaces when all else fails.
           (catch-all parent-bol ruby-indent-level))))
    `((ruby . ,common))))

(defun ruby-ts--block-indent-anchor (block-node-getter)
  (lambda (node parent _bol &rest _rest)
    (let ((block-node (funcall block-node-getter node parent)))
      (save-excursion
        (goto-char
         (treesit-node-start
          (if ruby-block-indent
              (ruby-ts--statement-ancestor block-node)
            block-node)))
        (back-to-indentation)
        (point)))))

(defun ruby-ts--binary-indent-anchor (_node parent _bol &rest _)
  (save-excursion
    (goto-char (treesit-node-start parent))
    (when (string-match-p ruby-ts--statement-container-regexp
                          (treesit-node-type (treesit-node-parent parent)))
      ;; Hack alert: it's not the proper place to alter the offset.
      ;; Redoing the analysis in the OFFSET form seems annoying,
      ;; though. (**)
      (forward-char ruby-indent-level))
    (point)))

(defun ruby-ts--parent-call-or-bol (_not parent _bol &rest _)
  (let* ((parent-bol (save-excursion
                       (goto-char (treesit-node-start parent))
                       (back-to-indentation)
                       (point)))
         (found
          (treesit-parent-until
           parent
           (lambda (node)
             (or (< (treesit-node-start node) parent-bol)
                 (string-match-p "\\`array\\|hash\\'" (treesit-node-type node))
                 ;; Method call on same line.
                 (equal (treesit-node-type node) "argument_list"))))))
    (cond
     ((null found)
      parent-bol)
     ;; No paren/curly/brace found on the same line.
     ((< (treesit-node-start found) parent-bol)
      parent-bol)
     ;; Nesting of brackets args.
     ((and
       (not (eq ruby-bracketed-args-indent t))
       (string-match-p "\\`array\\|hash\\'" (treesit-node-type parent))
       (or (equal (treesit-node-parent parent) found)
           ;; When the array/hash is part of a pair (keyword argument),
           ;; check if the pair's parent is the found node.
           (and (equal (treesit-node-type (treesit-node-parent parent)) "pair")
                (equal (treesit-node-parent (treesit-node-parent parent)) found)))
       ;; Grandparent is not a parenless call.
       (or (not (equal (treesit-node-type found) "argument_list"))
           (equal (treesit-node-type (treesit-node-child found 0))
                  "(")))
      parent-bol)
     ;; Hash or array opener on the same line.
     ((string-match-p "\\`array\\|hash\\'" (treesit-node-type found))
      (save-excursion
        (goto-char (treesit-node-start (treesit-node-child found 1)))
        (point)))
     ;; Parenless call found: indent to stmt with offset.
     ((not ruby-parenless-call-arguments-indent)
      (save-excursion
        (goto-char (treesit-node-start
                    (ruby-ts--statement-ancestor found)))
        ;; (**) Same.
        (+ (point) ruby-indent-level)))
     ;; Call with parens -- ident to first arg.
     ((equal (treesit-node-type (treesit-node-child found 0))
             "(")
      (save-excursion
        (goto-char (treesit-node-start (treesit-node-child found 1)))
        (point)))
     ;; Indent to the parenless call args beginning.
     (t
      (save-excursion
        (goto-char (treesit-node-start found))
        (point))))))

(defun ruby-ts--after-op-indent-p (&rest _)
  ruby-after-operator-indent)

(defun ruby-ts--method-call-indent-p (&rest _)
  ruby-method-call-indent)

(defun ruby-ts--class-or-module-p (node)
  "Predicate if NODE is a class or module."
  (string-match-p ruby-ts--class-or-module-regex (treesit-node-type node)))

(defun ruby-ts--get-name (node)
  "Return the text of the `name' field of NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun ruby-ts--full-name (node)
  "Return the fully qualified name of NODE."
  (let* ((name (ruby-ts--get-name node))
         (delimiter "#"))
    (when (equal (treesit-node-type node) "singleton_method")
      (setq delimiter "."
            name (treesit-node-text (treesit-node-child-by-field-name node "name"))))
    (while (setq node (treesit-parent-until node #'ruby-ts--class-or-module-p))
      (if name
          (setq name (concat (ruby-ts--get-name node) delimiter name))
        (setq name (ruby-ts--get-name node)))
      (setq delimiter "::"))
    name))

(defun ruby-ts--imenu-helper (tree)
  "Convert a treesit sparse tree NODE in a flat imenu list."
  (if (cdr tree)
      ;; We only use the "leaf" values in the tree.  It does include a
      ;; leaf node for every class or module body.
      (cl-mapcan #'ruby-ts--imenu-helper (cdr tree))
    (list (cons (ruby-ts--full-name (car tree))
                (treesit-node-start (car tree))))))

;; For now, this is going to work like ruby-mode and return a list of
;; class, modules, def (methods), and alias.  It is likely that this
;; can be rigged to be easily extended.
(defun ruby-ts--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root (treesit-buffer-root-node))
         (tree (treesit-induce-sparse-tree root
                                           (rx bol (or "singleton_method"
                                                       "method"
                                                       "alias"
                                                       "class"
                                                       "module")
                                               eol))))
    (ruby-ts--imenu-helper tree)))

(defun ruby-ts--arrow-up-start (arg)
  "Move to the start ARG levels up or out."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((pnt (point))
         (found (treesit-node-at pnt))
         (pos (treesit-node-start found))
         new-pos)
    (while (and found pos (> arg 0))
      (setq found (treesit-node-parent found)
            new-pos (treesit-node-start found))
      (when (and new-pos (not (= new-pos pos)))
        (setq arg (1- arg)
              pos new-pos)))
    (if pos
        (goto-char pos)
      (error "Something didn't work"))))

(defun ruby-ts--class-name (node)
  "Return NODE's name.
Assumes NODE's type is \"class\" or \"method\""
  (list
   (treesit-node-text
    (treesit-node-child-by-field-name
     node
     (if (equal "singleton_class" (treesit-node-type node)) "value" "name"))
    t)))

(defun ruby-ts--method-name (node)
  "Return the method name of NODE.
Assumes NODE's type is method or singleton_method."
  (if (equal "method" (treesit-node-type node))
      (list (treesit-node-text (treesit-node-child-by-field-name node "name") t))
    (let* ((children (treesit-node-children node))
           ;; 0th is "def"
           (first (nth 1 children))
           (third (nth 3 children)))
      (cond
       ((equal "(" (treesit-node-type first))
        (list (treesit-node-text (nth 2 children) t)
              (treesit-node-text (nth 5 children) t)))
       ;; ((equal "self" (treesit-node-type first))
       ;;  (list (treesit-node-text third t)))
       (t (mapcar (lambda (n)
                    (treesit-node-text n t))
                  (list first third)))))))

(defun ruby-ts-add-log-current-function ()
  "Return the current method name as a string.
The hash (#) is for instance methods only which are methods
\"defined on a class\" -- which is 99% of methods.  Otherwise, a
dot (.) is used.  Double colon (::) is used between classes.  The
leading double colon is not added."
  (let* ((node (treesit-node-at (point)))
         (method-pred
          (lambda (node)
            (and (<= (treesit-node-start node) (point))
                 (>= (treesit-node-end node) (point))
                 (string-match-p ruby-ts--method-regex (treesit-node-type node)))))
         (method (treesit-parent-until node method-pred t))
         (class (or method node))
         (result nil)
         (sep "#")
         (method-list nil)
         (class-list nil)
         (method-name nil))

    (when method
      (setq method-list (ruby-ts--method-name method))
      (unless (= 1 (length method-list))
        (setq sep ".")))
    (while (setq class (treesit-parent-until class
                                             (ruby-ts--type-pred
                                              ruby-ts--class-or-module-regex)))
      (setq class-list (append (ruby-ts--class-name class) class-list)))
    (setq method-name (car (last method-list))
          method-list (butlast method-list))
    (when (equal (car method-list) (car (last class-list)))
      (setq method-list (cdr method-list)))
    (dolist (ele (append class-list method-list))
      (cond
       ((equal "self" ele)
        (setq sep "."))
       ((string-match-p "\\`[^A-Z]" ele) ;not a class
        (setq sep "."
              result (if result
                         (concat result "::" ele)
                       ele)))
       (t (setq result (if result
                           (concat result "::" ele)
                         ele)))))
    (if method-name
        (concat result sep method-name)
      result)))

(defvar ruby-ts--s-p-query
  (when (treesit-available-p)
    (treesit-query-compile 'ruby
                           '(((heredoc_body) @heredoc)
                             ;; $' $" $`.
                             ((global_variable) @global_var
                              (:match "\\`\\$[#\"'`:?]" @global_var))
                             ;; ?' ?" ?` are character literals.
                             ((character) @char
                              (:match "\\`\\?[#\"'`:?]" @char))
                             ;; Symbols like :+, :<=> or :foo=.
                             ((simple_symbol) @symbol
                              (:match "\\s." @symbol))
                             ;; Method calls with name ending with ? or !.
                             ((call method: (identifier) @ident)
                              (:match "[?!]\\'" @ident))
                             ;; Method definitions for the above.
                             ((method name: (identifier) @ident)
                              (:match "[?!]\\'" @ident))
                             ;; Backtick method redefinition.
                             ((operator "`" @backtick))
                             ;; TODO: Stop at interpolations.
                             ((regex "/" @regex_slash))
                             ;; =begin...=end
                             ((comment) @comm
                              (:match "\\`=" @comm))
                             ;; Percent literals: %w[], %q{}, ...
                             ((string) @percent
                              (:match "\\`%" @percent))))))

(defun ruby-ts--syntax-propertize (beg end)
  (let ((captures (treesit-query-capture 'ruby ruby-ts--s-p-query beg end)))
    (pcase-dolist (`(,name . ,node) captures)
      (pcase-exhaustive name
        ('regex_slash
         ;; N.B.: A regexp literal with modifiers actually includes them in
         ;; the trailing "/" node.
         (put-text-property (treesit-node-start node) (1+ (treesit-node-start node))
                            'syntax-table
                            ;; Differentiate the %r{...} literals.
                            (if (eq ?/ (char-after (treesit-node-start node)))
                                (string-to-syntax "\"/")
                              (string-to-syntax "|"))))
        ('ident
         (put-text-property (1- (treesit-node-end node)) (treesit-node-end node)
                            'syntax-table (string-to-syntax "_")))
        ('symbol
         (goto-char (treesit-node-end node))
         (skip-syntax-backward "." (treesit-node-start node))
         (put-text-property (point) (treesit-node-end node)
                            'syntax-table (string-to-syntax "_")))
        ('heredoc
         (put-text-property (treesit-node-start node) (1+ (treesit-node-start node))
                            'syntax-table (string-to-syntax "\""))
         (when (< (treesit-node-end node) (point-max))
           (put-text-property (treesit-node-end node) (1+ (treesit-node-end node))
                              'syntax-table (string-to-syntax "\""))))
        ('percent
         ;; FIXME: Put the first one on the first paren in both %Q{} and %().
         ;; That would stop electric-pair-mode from pairing, though.  Hmm.
         (put-text-property (treesit-node-start node) (1+ (treesit-node-start node))
                            'syntax-table (string-to-syntax "|"))
         (put-text-property (1- (treesit-node-end node)) (treesit-node-end node)
                            'syntax-table (string-to-syntax "|")))
        ((or 'global_var 'char)
         (put-text-property (treesit-node-start node) (1+ (treesit-node-start node))
                            'syntax-table (string-to-syntax "'"))
         (put-text-property (1+ (treesit-node-start node)) (treesit-node-end node)
                            'syntax-table (string-to-syntax "_")))
        ('backtick
         (put-text-property (treesit-node-start node) (treesit-node-end node)
                            'syntax-table (string-to-syntax "_")))
        ('comm
         (dolist (pos (list (treesit-node-start node)
                            (1- (treesit-node-end node))))
           (put-text-property pos (1+ pos) 'syntax-table
                              (string-to-syntax "!"))))))))

(defun ruby-ts--sexp-p (node)
  ;; Skip parenless calls (implicit parens are both non-obvious to the
  ;; user, and might take over when we want to just over some physical
  ;; parens/braces).
  (or (not (equal (treesit-node-type node)
                  "argument_list"))
      (equal (treesit-node-type (treesit-node-child node 0))
             "(")))

(defun ruby-ts--list-p (node)
  ;; Distinguish between the named `unless' node and the
  ;; node with the same value of type.
  (when (treesit-node-check node 'named)
    (ruby-ts--sexp-p node)))

(defvar-keymap ruby-ts-mode-map
  :doc "Keymap used in Ruby mode"
  :parent prog-mode-map
  ;; (when ruby-use-smie
  ;;   (define-key map (kbd "M-C-d") 'smie-down-list))
  ;; (define-key map (kbd "M-C-p") 'ruby-beginning-of-block)
  ;; (define-key map (kbd "M-C-n") 'ruby-end-of-block)
  "C-c {" #'ruby-toggle-block
  "C-c '" #'ruby-toggle-string-quotes
  "C-c C-f" #'ruby-find-library-file)

;;;###autoload
(define-derived-mode ruby-ts-mode ruby-base-mode "Ruby"
  "Major mode for editing Ruby, powered by tree-sitter."
  :group 'ruby
  :syntax-table ruby-mode-syntax-table

  (unless (treesit-ensure-installed 'ruby)
    (error "Tree-sitter for Ruby isn't available"))

  (setq treesit-primary-parser (treesit-parser-create 'ruby))

  (setq-local add-log-current-defun-function #'ruby-ts-add-log-current-function)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp ruby-ts--method-regex)

  (setq-local treesit-thing-settings
              `((ruby
                 (sexp (not (or (and named
                                     ,(rx bos (or "program"
                                                  "body_statement"
                                                  "comment"
                                                  "then")
                                          eos))
                                (and anonymous
                                     ,(rx bos (or "do" "begin"
                                                  "if" "unless"
                                                  "def" "end"
                                                  "(" ")" "[" "]"
                                                  "{" "}" "|" "," ";")
                                          eos)))))
                 (list ,(cons (rx
                               bos
                               (or
                                "begin_block"
                                "end_block"
                                "method"
                                "singleton_method"
                                "method_parameters"
                                "parameters"
                                "block_parameters"
                                "class"
                                "singleton_class"
                                "module"
                                "do"
                                "case"
                                "case_match"
                                "array_pattern"
                                "find_pattern"
                                "hash_pattern"
                                "parenthesized_pattern"
                                "expression_reference_pattern"
                                "if"
                                "unless"
                                "begin"
                                "parenthesized_statements"
                                "argument_list"
                                "do_block"
                                "block"
                                "destructured_left_assignment"
                                "interpolation"
                                "string"
                                "string_array"
                                "symbol_array"
                                "delimited_symbol"
                                "regex"
                                "heredoc_body"
                                "array"
                                "hash")
                               eos)
                              #'ruby-ts--list-p))
                 (sexp-default
                  ;; For `C-M-f' in "#|{a}"
                  ("#{" . ,(lambda (node)
                             (and (eq (char-after (point)) ?{)
                                  (equal (treesit-node-type (treesit-node-parent node))
                                         "interpolation")))))
                 (sentence ,(rx bos (or "return"
                                        "body_statement"
                                        "call"
                                        "assignment")
                                eos))
                 (text ,(lambda (node)
                          (or (member (treesit-node-type node)
                                      '("comment" "string_content"
                                        "heredoc_content"))))))))

  ;; Imenu.
  (setq-local imenu-create-index-function #'ruby-ts--imenu)

  ;; Outline minor mode.
  (setq-local treesit-outline-predicate
              `(and ,(rx bos (or "singleton_method"
                                 "method"
                                 "alias"
                                 "singleton_class"
                                 "class"
                                 "module")
                         eos)
                    named))
  ;; Restore default values of outline variables
  ;; to use `treesit-outline-predicate'.
  (kill-local-variable 'outline-regexp)
  (kill-local-variable 'outline-level)

  (setq-local treesit-simple-indent-rules (ruby-ts--indent-rules))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (ruby-ts--font-lock-settings 'ruby))
  ;; Level 3 is the default.
  (setq-local treesit-font-lock-feature-list
              '(( comment method-definition parameter-definition)
                ( keyword regexp string type)
                ( builtin-variable builtin-constant builtin-function
                  delimiter escape-sequence
                  constant global instance
                  interpolation literal symbol assignment)
                ( bracket error function operator punctuation)))

  (treesit-major-mode-setup)

  (setq-local syntax-propertize-function #'ruby-ts--syntax-propertize))

(derived-mode-add-parents 'ruby-ts-mode '(ruby-mode))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(ruby-mode . ruby-ts-mode)))

(provide 'ruby-ts-mode)

;;; ruby-ts-mode.el ends here
