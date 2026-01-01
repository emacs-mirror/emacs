;;; c-ts-mode.el --- tree-sitter support for C and C++  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : c c++ cpp languages tree-sitter

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
;; c-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-c: v0.23.4-1-g3aa2995
;;
;; c++-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-cpp: v0.23.4-1-gf41b4f6
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:
;;
;; This package provides major modes for C and C++, plus some handy
;; functions that are useful generally to major modes for C-like
;; languages.
;;
;; This package provides `c-ts-mode' for C, `c++-ts-mode' for C++, and
;; `c-or-c++-ts-mode' which automatically chooses the right mode for
;; C/C++ header files.
;;
;; To use these modes by default, assuming you have the respective
;; tree-sitter grammars available, do one of the following:
;;
;; - Add one or more of the following lines to your init file:
;;
;;    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
;;
;;   If you have only C grammar available, use only the first one; if
;;   you have only the C++ grammar, use only the second one.
;;
;; - Customize 'auto-mode-alist' to turn one or more of the modes
;;   automatically.  For example:
;;
;;     (add-to-list 'auto-mode-alist
;;                  '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
;;                    . c++-ts-mode))
;;
;;   will turn on the c++-ts-mode for C++ source files.
;;
;; - If you have both C and C++ grammars installed, customize
;;   'treesit-enabled-modes' and select 'c-ts-mode',
;;   'c++-mode', 'c-or-c++-mode'.
;;
;; You can also turn on these modes manually in a buffer.  Doing so
;; will set up Emacs to use the C/C++ modes defined here for other
;; files, provided that you have the corresponding parser grammar
;; libraries installed.
;;
;; If the tree-sitter doxygen grammar is available, then the comment
;; blocks can be highlighted according to this grammar.

;;; Code:

(require 'treesit)
(require 'c-ts-common)
(eval-when-compile (require 'rx))
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(c "https://github.com/tree-sitter/tree-sitter-c"
     :commit "3aa2995549d5d8b26928e8d3fa2770fd4327414e")
 t)
(add-to-list
 'treesit-language-source-alist
 '(cpp "https://github.com/tree-sitter/tree-sitter-cpp"
       :commit "f41b4f66a42100be405f96bdc4ebc4a61095d3e8")
 t)
(add-to-list
 'treesit-language-source-alist
 '(doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
           :commit "1e28054cb5be80d5febac082706225e42eff14e6")
 t)

;;; Custom variables

(defcustom c-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `c-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'c)

(defun c-ts-mode-toggle-comment-style (&optional arg)
  "Toggle the comment style between block and line comments.
Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or omitted."
  (interactive "P")
  (let ((prevstate-line (string= comment-start "// ")))
    (when (or (not arg)
              (zerop (setq arg (prefix-numeric-value arg)))
              (xor (> 0 arg) prevstate-line))
      (pcase-let ((`(,starter . ,ender)
                   (if prevstate-line
                       (cons "/* " " */")
                     (cons "// " ""))))
        (setq-local comment-start starter
                    comment-end ender))
      (c-ts-mode-set-modeline))))

(defun c-ts-mode-set-modeline ()
  "Set major mode name to either C or C++ in mode-line."
  (setq mode-name
        (concat (if (eq major-mode 'c-ts-mode) "C" "C++")
                (string-trim-right comment-start)))
  (force-mode-line-update))

(defun c-ts-mode--indent-style-setter (sym val)
  "Custom setter for `c-ts-mode-set-style'.

Apart from setting the default value of SYM to VAL, also change
the value of SYM in `c-ts-mode' and `c++-ts-mode' buffers to VAL.

SYM should be `c-ts-mode-indent-style', and VAL should be a style
symbol."
  (set-default sym val)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p '(c-ts-mode c++-ts-mode))
        (setq-local c-ts-mode-indent-style val)
        (c-ts-mode-set-style val)))))

(defun c-ts-indent-style-safep (style)
  "Non-nil if STYLE's value is safe for file-local variables."
  (and (symbolp style) (not (functionp style))))

(defcustom c-ts-mode-indent-style 'gnu
  "Style used for indentation.

The selected style could be one of GNU, K&R, LINUX or BSD.  If the
supplied styles don't suffice, the value could be a function instead.
This function takes no arguments and is expected to return a list of
indent RULEs as described in `treesit-simple-indent-rules'.  Note that
the list of RULEs doesn't need to contain the language symbol."
  :version "29.1"
  :type '(choice (const :tag "Gnu" gnu)
                 (const :tag "K&R" k&r)
                 (const :tag "Linux" linux)
                 (const :tag "BSD" bsd)
                 (function :tag "A function for user customized style" ignore))
  :set #'c-ts-mode--indent-style-setter
  :safe 'c-ts-indent-style-safep
  :group 'c)

(defun c-ts-mode--prompt-for-style ()
  "Prompt for an indent style and return the symbol for it."
  (intern
   (completing-read
    "Style: "
    '(gnu k&r linux bsd)
    nil t nil nil "gnu")))

(defun c-ts-mode-set-global-style (style)
  "Set the indent style of C/C++ modes globally to STYLE.

This changes the current indent style of every C/C++ buffer and
the default C/C++ indent style for `c-ts-mode' and `c++-ts-mode'
in this Emacs session."
  (interactive (list (c-ts-mode--prompt-for-style)))
  (c-ts-mode--indent-style-setter 'c-ts-mode-indent-style style))

(defun c-ts-mode-set-style (style)
  "Set the C/C++ indent style of the current buffer to STYLE.

To set the default indent style globally, use
`c-ts-mode-set-global-style'."
  (interactive (list (c-ts-mode--prompt-for-style)))
  (if (not (derived-mode-p '(c-ts-mode c++-ts-mode)))
      (user-error "The current buffer is not in `c-ts-mode' nor `c++-ts-mode'")
    (setq-local c-ts-mode-indent-style style)
    (setq treesit-simple-indent-rules
          (if (functionp style)
              (funcall style)
            (c-ts-mode--simple-indent-rules
             (if (derived-mode-p 'c-ts-mode) 'c 'cpp)
             style)))))

(defcustom c-ts-mode-emacs-sources-support t
  "Whether to enable Emacs source-specific C features.
This enables detection of definitions of Lisp functions via
the DEFUN macro.
This needs to be set before enabling `c-ts-mode'; if you change
the value after enabling `c-ts-mode', toggle the mode off and on
again."
  :version "29.1"
  :type 'boolean
  :safe 'booleanp
  :group 'c)

(defcustom c-ts-mode-enable-doxygen nil
  "Enable doxygen syntax highlighting.
If Non-nil, enable doxygen based font lock for comment blocks.
This needs to be set before enabling `c-ts-mode'; if you change
the value after enabling `c-ts-mode', toggle the mode off and on
again."
  :version "31.1"
  :type 'boolean
  :safe 'booleanp
  :group 'c)

;;; Syntax table

(defvar c-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table for `c-ts-mode'.")

(defun c-ts-mode--syntax-propertize (beg end)
  "Apply syntax text property to template delimiters between BEG and END.

< and > are usually punctuation, e.g., in ->.  But when used for
templates, they should be considered pairs.

This function checks for < and > in the changed RANGES and applies
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
            (treesit-node-parent
             (treesit-node-at (match-beginning 0))))
      ("template_argument_list"
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'syntax-table
                          (pcase (char-before)
                            (?< '(4 . ?>))
                            (?> '(5 . ?<))))))))

;;; Indent

(defun c-ts-mode--preproc-offset (_n _p &rest _)
  "This anchor is used for preprocessor directives.

Because node is nil at the moment of indentation, we use
`treesit-node-on' to capture the anonymous node covering the
newline.  If the grand-parent of that node is the
translation_unit itself, we don't indent.  Otherwise, just indent
one step according to the great-grand-parent indent level.  The
reason there is a difference between grand-parent and
great-grand-parent here is that the node containing the newline
is actually the parent of point at the moment of indentation."
  (when-let* ((node (treesit-node-on (point) (point))))
    (if (string-equal "translation_unit"
                      (treesit-node-type
                       (treesit-node-parent
                        (treesit-node-parent node))))
        0
      c-ts-mode-indent-offset)))

(defun c-ts-mode--prev-sibling (node parent bol &rest _)
  "Return the start of the previous named sibling of NODE.

This anchor handles the special case where the previous sibling is a
labeled_statement or preproc directive; in that case, return the child
of the labeled statement instead.  (Actually, recursively go down until
the node isn't a labeled_statement or preproc.)  E.g.,

label:
  int x = 1;
  int y = 2;

The anchor of \"int y = 2;\" should be \"int x = 1;\" rather than
the labeled_statement.

Return nil if a) there is no prev-sibling, or b) prev-sibling
doesn't have a child.

PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters of the current line."
  (when-let* ((prev-sibling
               (or (treesit-node-prev-sibling node t)
                   (treesit-node-prev-sibling
                    (treesit-node-first-child-for-pos parent bol) t)
                   (treesit-node-child parent -1 t)))
              (continue t))
    (save-excursion
      (while (and prev-sibling continue)
        (pcase (treesit-node-type prev-sibling)
          ;; Get the statement in the label.
          ("labeled_statement"
           (setq prev-sibling (treesit-node-child prev-sibling 2)))
          ;; Get the last statement in the preproc.  Tested by
          ;; "Prev-Sibling When Prev-Sibling is Preproc" test.
          ((or "preproc_if" "preproc_ifdef")
           (setq prev-sibling (treesit-node-child prev-sibling -2)))
          ((or "preproc_elif" "preproc_else")
           (setq prev-sibling (treesit-node-child prev-sibling -1)))
          ((or "#elif" "#else")
           (setq prev-sibling (treesit-node-prev-sibling
                               (treesit-node-parent prev-sibling) t)))
          ;; If the start of the previous sibling isn't at the beginning
          ;; of a line, something's probably not quite right, go a step
          ;; further. (E.g., comment after a statement.)  If the
          ;; previous sibling is the first named node, then anchor to
          ;; that, e.g. when returning an aggregate and starting the
          ;; items on the same line as {.
          (_ (goto-char (treesit-node-start prev-sibling))
             (if (or (looking-back (rx bol (* whitespace))
                                   (line-beginning-position))
                     (treesit-node-eq (treesit-node-child parent 0 t)
                                      prev-sibling))
                 (setq continue nil)
               (setq prev-sibling
                     (treesit-node-prev-sibling prev-sibling)))))))
    ;; This could be nil if a) there is no prev-sibling or b)
    ;; prev-sibling doesn't have a child.
    (treesit-node-start prev-sibling)))

(defun c-ts-mode--standalone-parent (_n parent &rest _)
  "Like the standalone-parent anchor but skips preproc nodes and labels.
PARENT is the parent of the current node."
  (save-excursion
    (treesit-node-start
     (treesit-parent-until
      ;; Use PARENT rather than NODE, to handle the case where NODE is
      ;; nil.
      parent (lambda (node)
               (and node
                    (not (treesit-node-match-p
                          node (rx (or "preproc" "labeled_statement"))))
                    (progn
                      (goto-char (treesit-node-start node))
                      (looking-back (rx bol (* whitespace))
                                    (line-beginning-position)))))
      t))))

(defun c-ts-mode--for-loop-indent-rule (node parent &rest _)
  "Indentation rule for the for-loop.

NODE and PARENT as usual."
  (when (treesit-node-match-p parent "for_statement")
    ;; The first version of this function tests for the field name of
    ;; NODE, which is a lot cleaner.  Alas, older tree-sitter library
    ;; has a bug in treesit-node-field-name-for-child, which make it
    ;; give the wrong field name for a child node.
    (cond
     ;; Body (Check if NODE is the last child, because when
     ;; initializer/condition/update is empty, the index of body can
     ;; change). Eg, for (;;) {...}
     ((treesit-node-eq node (treesit-node-child parent -1 'named))
      (cons (c-ts-common--standalone-parent parent)
            c-ts-mode-indent-offset))
     ;; Initializer.
     ((and (treesit-node-check node 'named)
           (eq (treesit-node-index node 'named) 0 ))
      ;; Anchor is the opening paren.
      (cons (treesit-node-start (treesit-node-child parent 1)) 1))
     ;; Condition and update.
     ((and (treesit-node-check node 'named)
           (<= 1 (treesit-node-index node 'named) 2))
      (cons (treesit-node-start (treesit-node-prev-sibling node 'named))
            0))
     ((treesit-node-match-p node ")")
      ;; Anchor is the opening paren.
      (cons (treesit-node-start (treesit-node-child parent 1)) 0))
     (t nil))))

(defvar c-ts-mode--preproc-indent-rules
  `(((node-is "preproc") column-0 0)
    ((node-is "#endif") column-0 0)
    ((match "preproc_call" "compound_statement") column-0 0)
    ((prev-line-is "#endif") c-ts-mode--prev-sibling 0)
    ;; Top-level things under a preproc directive.  Note that
    ;; "preproc" matches more than one type: it matches
    ;; preproc_if, preproc_elif, etc.
    ((n-p-gp nil "preproc" "translation_unit") column-0 0)
    ;; Indent rule for an empty line after a preproc directive.
    ((and no-node (parent-is ,(rx (or "\n" "preproc"))))
     c-ts-mode--standalone-parent c-ts-mode--preproc-offset)
    ;; Statement under a preproc directive, the first statement
    ;; indents against parent, the rest statements indent to
    ;; their prev-sibling.
    ((match nil ,(rx "preproc_" (or "if" "elif")) nil 3 3)
     c-ts-mode--standalone-parent c-ts-mode-indent-offset)
    ((match nil "preproc_ifdef" nil 2 2)
     c-ts-mode--standalone-parent c-ts-mode-indent-offset)
    ((match nil "preproc_else" nil 1 1)
     c-ts-mode--standalone-parent c-ts-mode-indent-offset)
    ((parent-is "preproc") c-ts-mode--prev-sibling 0))
  "Indent rules for preprocessors.")

(defun c-ts-mode--macro-heuristic-rules (node parent &rest _)
  "Heuristic indent rule for control flow macros.

Eg,

    #define IOTA(var, n) for (int var = 0; var != (n); ++var)

    int main()
    {
      IOTA (v, 10) {
        printf(\"%d \", v);   <-- Here we want to indent
        counter++;            <-- Use baseline rule to align
    }                             to prev sibling

Checked by \"Compound Statement after code (Bug#74507)\" test.

NODE and PARENT are the same as other indent rules."
  (when (and (treesit-node-match-p parent "compound_statement")
             (treesit-node-match-p (treesit-node-prev-sibling parent)
                                   "expression_statement"))
    (let ((parent-bol
           (lambda () (save-excursion
                        (goto-char (treesit-node-start parent))
                        (back-to-indentation)
                        (point)))))
      (cond
       ;; Closing brace.
       ((treesit-node-match-p node "}")
        (cons (funcall parent-bol) 0))
       ;; First sibling.
       ((treesit-node-eq (treesit-node-child parent 0 'named) node)
        (cons (funcall parent-bol)
              c-ts-mode-indent-offset))))))

(defun c-ts-mode--emacs-macro-rules (_ parent &rest _)
  "Rules for indenting macros in Emacs C source.

PARENT is the same as other simple-indent rules."
  (cond
   ((and (treesit-node-match-p parent "function_definition")
         (equal (treesit-node-text
                 (treesit-node-child-by-field-name parent "type"))
                "FOR_EACH_TAIL"))
    (cons (treesit-node-start parent)
          c-ts-mode-indent-offset))))

(defun c-ts-mode--simple-indent-rules (mode style)
  "Return the indent rules for MODE and STYLE.

The returned value can be set to `treesit-simple-indent-rules'.
MODE can be `c' or `cpp'.  STYLE can be `gnu', `k&r', `linux', `bsd'."
  (let ((rules
         `((c-ts-mode--for-each-tail-body-matcher
            prev-line c-ts-mode-indent-offset)

           ;; Misc overrides.
           ((parent-is "translation_unit") column-0 0)
           ((node-is ,(rx (or "else" "case"))) standalone-parent 0)
           ;; Align the while keyword to the do keyword.
           ((match "while" "do_statement") parent 0)
           c-ts-mode--parenthesized-expression-indent-rule
           ;; Thanks to tree-sitter-c's weird for-loop grammar, we can't
           ;; use the baseline indent rule for it.
           c-ts-mode--for-loop-indent-rule
           c-ts-mode--label-indent-rules
           ,@c-ts-mode--preproc-indent-rules
           c-ts-mode--macro-heuristic-rules
           c-ts-mode--emacs-macro-rules

           ;; Make sure type and function definition components align and
           ;; don't indent. Also takes care of GNU style opening braces.
           ((parent-is ,(rx (or "function_definition"
                                "struct_specifier"
                                "enum_specifier"
                                "function_declarator"
                                "template_declaration")))
            standalone-parent 0)
           ;; This is for the trailing-star stype:  int *
           ;;                                       func()
           ((match "function_declarator" nil "declarator") parent-bol 0)
           ;; ((match nil "function_definition" "declarator") parent 0)
           ;; ((match nil "struct_specifier" "name") parent 0)
           ;; ((match nil "function_declarator" "parameters") parent 0)
           ;; ((parent-is "template_declaration") parent 0)

           ;; `c-ts-common-looking-at-star' has to come before
           ;; `c-ts-common-comment-2nd-line-matcher'.
           ;; FIXME: consolidate into a single rule.
           ((and (parent-is "comment") c-ts-common-looking-at-star)
            c-ts-common-comment-start-after-first-star -1)
           (c-ts-common-comment-2nd-line-matcher
            c-ts-common-comment-2nd-line-anchor
            1)
           ((parent-is "comment") prev-adaptive-prefix 0)

           ;; Preproc directives
           ((node-is "preproc_arg") no-indent)
           ((node-is "preproc") column-0 0)
           ((node-is "#endif") column-0 0)

           ;; C++
           ((node-is "access_specifier") parent-bol 0)
           ((prev-line-is "access_specifier")
            parent-bol c-ts-mode-indent-offset)

           c-ts-common-baseline-indent-rule)))
    (setq rules
          (pcase style
            ('gnu rules)
            ('k&r rules)
            ('linux
             ;; Reference:
             ;; https://www.kernel.org/doc/html/latest/process/coding-style.html,
             ;; and script/Lindent in Linux kernel repository.
             `(((node-is "labeled_statement") column-0 0)
               ,@rules))
            ('bsd
             `(((match "compound_statement" "compound_statement")
                standalone-parent c-ts-mode-indent-offset)
               ((node-is "compound_statement") standalone-parent 0)
               ,@rules))))
    (pcase mode
      ('c `((c . ,rules)))
      ('cpp `((cpp . ,rules))))))

(defun c-ts-mode--parenthesized-expression-indent-rule (_node parent &rest _)
  "Indent rule that indents parenthesized expression.

Aligns the next line to the first sibling

return (a && b
          && c)

return (  a && b
          && c
          )

Same for if/while statements

if (a && b
      && c)

NODE, PARENT are the same as other indent rules."
  (when (treesit-node-match-p
         parent (rx (or "binary" "conditional") "_expression"))
    (while (and parent
                (not (treesit-node-match-p
                      parent "parenthesized_expression")))
      (setq parent (treesit-node-parent parent)))
    (when parent
      (cons (treesit-node-start
             (treesit-node-child parent 0 'named))
            0))))

(defun c-ts-mode--label-indent-rules (node parent bol &rest args)
  "Handles indentation around labels.
NODE, PARENT, BOL, ARGS are as usual."
  (cond
   ;; Matches the top-level labels for GNU-style.
   ((and (eq c-ts-mode-indent-style 'gnu)
         (treesit-node-match-p node "labeled_statement")
         (treesit-node-match-p (treesit-node-parent parent)
                               "function_definition"))
    (cons (pos-bol) 1))
   ;; Indent the label itself.
   ((treesit-node-match-p node "labeled_statement")
    (cons (c-ts-mode--standalone-parent node parent bol args)
          0))
   ;; Indent the statement below the label.
   ((treesit-node-match-p parent "labeled_statement")
    (cons (c-ts-mode--standalone-parent node parent bol args)
          c-ts-mode-indent-offset))
   ;; If previous sibling is a labeled_statement, align to it's
   ;; children, which is the previous statement.
   ((and (not (treesit-node-match-p node "}"))
         (treesit-node-match-p (treesit-node-prev-sibling node)
                               "labeled_statement"))
    (cons (c-ts-mode--prev-sibling node parent bol args)
          0))
   (t nil)))

;;; Font-lock

(defvar c-ts-mode--feature-list
  '(( comment document definition)
    ( keyword preprocessor string type)
    ( assignment constant escape-sequence label literal)
    ( bracket delimiter error function operator property variable))
  "`treesit-font-lock-feature-list' for `c-ts-mode'.")

(defvar c-ts-mode--preproc-keywords
  '("#define" "#if" "#ifdef" "#ifndef"
    "#else" "#elif" "#endif" "#include")
  "C/C++ keywords for tree-sitter font-locking.")

(defvar c-ts-mode--optional-c-keywords
  ;; v0.20.4 actually contains all the new keywords I can find that
  ;; aren't in c-ts-mode before.  The version doesn't really matter, we
  ;; just want a rough grouping so that we can enable as much keywords
  ;; as possible.
  '(("v0.20.4" . ("_Generic"  "_Noreturn" "noreturn"
                  "__attribute__" "__restrict__"
                  "offsetof" "thread_local"))
    ("v0.20.5" . ("__extension__" "__extension__"
                  "__forceinline" "__inline" "__inline__"
                  "__thread"
                  "__alignof__" "__alignof" "alignof" "_Alignof"))
    ("v0.20.7" . ("__try" "__except" "__finally" "__leave"))
    ;; GitHub release jumped from v0.20.7 to v0.23.1.  There are
    ;; intermediate git tags, but there aren't many keywords here, so I
    ;; skipped intermediate versions too.
    ("v0.23.1" ("_Nonnull" "__attribute"
                "alignas" "_Alignas" "__asm" "__volatile__"))
    ;; No changes to keywords in v0.23.2, v0.23.3, v0.23.4.
    )
  "Keywords added in each tree-sitter-c version.")

(defvar c-ts-mode--ms-keywords
  ;; For some reason, "__restrict" "__uptr" and "__sptr" are not
  ;; recognized by the grammar, although being in grammar.js.
  '("__declspec" "__based" "__cdecl" "__clrcall" "__stdcall"
    "__fastcall" "__thiscall" "__vectorcall" "_unaligned" "__unaligned")
  "MSVC keywords.")

(defvar c-ts-mode--type-keywords
  '("long" "short" "signed" "unsigned")
  "Keywords that should be considered as part of a type.")

(defvar c-ts-mode--operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++")
  "C/C++ operators for tree-sitter font-locking.")

(defvar c-ts-mode--c++-operators
  '(".*" "->*" "<=>")
  "C++ operators that aren't supported by C.")

(defvar c-ts-mode--c++-operator-keywords
  '("and" "and_eq" "bitand" "bitor" "compl" "not" "not_eq" "or" "or_eq"
    "xor" "xor_eq")
  "C++ operators that we fontify as keywords.")

(defun c-ts-mode--compute-optional-keywords (mode)
  "Return a list of keywords that are supported by the grammar.
MODE should be either `c' or `cpp'."
  (if (eq mode 'c)
      (mapcan
       (lambda (entry)
         (let ((keywords (cdr entry)))
           (if (treesit-query-valid-p 'c `([,@keywords] @cap))
               (copy-sequence keywords)
             nil)))
       c-ts-mode--optional-c-keywords)
    ;; As for now, there aren't additional optional keywords for C++.
    ()))

(defun c-ts-mode--keywords (mode)
  "C/C++ keywords for tree-sitter font-locking.
MODE is either `c' or `cpp'."
  (let ((c-keywords
         `("_Atomic" "break" "case" "const" "continue"
           "default" "do" "else" "enum"
           "extern" "for" "goto" "if" "inline"
           "register" "restrict" "return"
           "sizeof" "static" "struct"
           "switch" "typedef" "union"
           "volatile" "while"
           ,@c-ts-mode--ms-keywords
           ,@(c-ts-mode--compute-optional-keywords mode))))
    (if (eq mode 'cpp)
        (append c-keywords
                c-ts-mode--c++-operator-keywords
                '("catch" "class" "co_await" "co_return"
                  "co_yield" "concept" "consteval"
                  "constexpr" "constinit" "decltype" "delete"
                  "explicit" "final" "friend"
                  "mutable" "namespace" "new" "noexcept"
                  "operator" "override" "private" "protected"
                  "public" "requires" "static_assert" "template" "throw"
                  "try" "typename" "using"
                  "thread_local"))
      (append '("auto") c-keywords))))

(defvar c-ts-mode--for-each-tail-regexp
  (rx "FOR_EACH_" (or "TAIL" "TAIL_SAFE" "ALIST_VALUE"
                      "LIVE_BUFFER" "FRAME"))
  "A regexp matching all the variants of the FOR_EACH_* macro.")

(defvar c-ts-mode--doxygen-comment-regex
  (rx (| "/**" "/*!" "//!" "///"))
  "A regexp that matches all doxygen comment styles.")

(defun c-ts-mode--font-lock-settings (mode)
  "Tree-sitter font-lock settings.
MODE is either `c' or `cpp'."
  (treesit-font-lock-rules
   :default-language mode
   :feature 'comment
   `(((comment) @font-lock-doc-face
      (:match ,(rx bos "/**") @font-lock-doc-face))
     (comment) @font-lock-comment-face)

   :feature 'preprocessor
   `((preproc_directive) @font-lock-preprocessor-face

     (preproc_def
      name: (identifier) @font-lock-variable-name-face)

     (preproc_ifdef
      name: (identifier) @font-lock-variable-name-face)

     (preproc_function_def
      name: (identifier) @font-lock-function-name-face)

     (preproc_params
      (identifier) @font-lock-variable-name-face)

     (preproc_defined
      "defined" @font-lock-preprocessor-face
      "(" @font-lock-preprocessor-face
      (identifier) @font-lock-variable-name-face
      ")" @font-lock-preprocessor-face)
     [,@c-ts-mode--preproc-keywords] @font-lock-preprocessor-face)

   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face)

   :feature 'keyword
   `([,@(c-ts-mode--keywords mode)] @font-lock-keyword-face
     ,@(when (eq mode 'cpp)
         '((auto) @font-lock-keyword-face
           (this) @font-lock-keyword-face))
     ,@(when (eq mode 'cpp)
         (treesit-query-first-valid 'cpp
           '((virtual) @font-lock-keyword-face)
           '("virtual" @font-lock-keyword-face))))

   :feature 'operator
   `([,@c-ts-mode--operators
      ,@(when (eq mode 'cpp) c-ts-mode--c++-operators)]
     @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :feature 'string
   `((string_literal) @font-lock-string-face
     (system_lib_string) @font-lock-string-face
     ,@(when (eq mode 'cpp)
         '((raw_string_literal) @font-lock-string-face)))

   :feature 'literal
   `((number_literal) @font-lock-number-face
     (char_literal) @font-lock-constant-face)

   :feature 'type
   `((primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     (sized_type_specifier) @font-lock-type-face
     ,@(when (eq mode 'cpp)
         '((type_qualifier) @font-lock-type-face

           (qualified_identifier
            scope: (namespace_identifier) @font-lock-constant-face)

           (operator_cast) type: (type_identifier) @font-lock-type-face

           (namespace_identifier) @font-lock-constant-face))
     [,@c-ts-mode--type-keywords] @font-lock-type-face)

   :feature 'definition
   ;; Highlights identifiers in declarations.
   `(,@(when (eq mode 'cpp)
         '((destructor_name (identifier) @font-lock-function-name-face)))
     (declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (field_declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (function_definition
      declarator: (_) @c-ts-mode--fontify-declarator)
     ;; When a function definition has preproc directives in its body,
     ;; it can't correctly parse into a function_definition.  We still
     ;; want to highlight the function_declarator correctly, hence
     ;; this rule.  See bug#63390 for more detail.
     ((function_declarator) @c-ts-mode--fontify-declarator
      (:pred c-ts-mode--top-level-declarator
             @c-ts-mode--fontify-declarator))

     (parameter_declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (enumerator
      name: (identifier) @font-lock-property-name-face))

   :feature 'assignment
   ;; TODO: Recursively highlight identifiers in parenthesized
   ;; expressions, see `c-ts-mode--fontify-declarator' for
   ;; inspiration.
   '((assignment_expression
      left: (identifier) @font-lock-variable-use-face)
     (assignment_expression
      left: (field_expression field: (_) @font-lock-property-use-face))
     (assignment_expression
      left: (pointer_expression
             (identifier) @font-lock-variable-use-face))
     (assignment_expression
      left: (subscript_expression
             (identifier) @font-lock-variable-use-face))
     (init_declarator declarator: (_) @c-ts-mode--fontify-declarator))

   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-call-face
       (field_expression field: (field_identifier) @font-lock-function-call-face)]))

   :feature 'variable
   '((identifier) @c-ts-mode--fontify-variable)

   :feature 'label
   '((labeled_statement
      label: (statement_identifier) @font-lock-constant-face))

   :feature 'error
   '((ERROR) @c-ts-mode--fontify-error)

   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'property
   '((field_identifier) @font-lock-property-use-face)

   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)

   :feature 'emacs-devel
   :override t
   `(((call_expression
       (call_expression function: (identifier) @fn)
       @c-ts-mode--fontify-DEFUN)
      (:match "\\`DEFUN\\'" @fn))

     ((function_definition type: (_) @for-each-tail)
      @c-ts-mode--fontify-for-each-tail
      (:match ,c-ts-mode--for-each-tail-regexp @for-each-tail)))))

;;; Font-lock helpers

(defun c-ts-mode--declarator-identifier (node &optional qualified)
  "Return the identifier of the declarator node NODE.

If QUALIFIED is non-nil, include the namespace part of the
identifier and return a qualified_identifier."
  (pcase (treesit-node-type node)
    ;; Recurse.
    ((or "attributed_declarator" "parenthesized_declarator")
     (c-ts-mode--declarator-identifier (treesit-node-child node 0 t)
                                       qualified))
    ((or "pointer_declarator" "reference_declarator")
     (c-ts-mode--declarator-identifier (treesit-node-child node -1)
                                       qualified))
    ((or "function_declarator" "array_declarator" "init_declarator")
     (c-ts-mode--declarator-identifier
      (treesit-node-child-by-field-name node "declarator")
      qualified))
    ("qualified_identifier"
     (if qualified
         node
       (c-ts-mode--declarator-identifier
        (treesit-node-child-by-field-name node "name")
        qualified)))
    ;; Terminal case.
    ((or "identifier" "field_identifier")
     node)))

(defun c-ts-mode--fontify-declarator (node override start end &rest _args)
  "Fontify a declarator (whatever is under the \"declarator\" field).
For NODE, OVERRIDE, START, END, and ARGS, see
`treesit-font-lock-rules'."
  (let* ((identifier (c-ts-mode--declarator-identifier node))
         (qualified-root
          (treesit-parent-while (treesit-node-parent identifier)
                                (lambda (node)
                                  (equal (treesit-node-type node)
                                         "qualified_identifier"))))
         (face (pcase (treesit-node-type (treesit-node-parent
                                          (or qualified-root
                                              identifier)))
                 ("field_declaration" 'font-lock-property-name-face)
                 ("function_declarator" 'font-lock-function-name-face)
                 (_ 'font-lock-variable-name-face))))
    (when identifier
      (treesit-fontify-with-override
       (treesit-node-start identifier) (treesit-node-end identifier)
       face override start end))))

(defun c-ts-mode--top-level-declarator (node)
  "Return non-nil if NODE is a top-level function_declarator."
  ;; These criterion are observed in
  ;; xterm.c:x_draw_glyphless_glyph_string_foreground on emacs-29
  ;; branch, described in bug#63390.  They might not cover all cases
  ;; where a function_declarator is at top-level, outside of a
  ;; function_definition.  We might need to amend them as we discover
  ;; more cases.
  (let* ((parent (treesit-node-parent node))
         (grandparent (treesit-node-parent parent)))
    (and (equal (treesit-node-type parent) "ERROR")
         (null grandparent))))

(defun c-ts-mode--fontify-variable (node override start end &rest _)
  "Fontify an identifier node if it is a variable.
Don't fontify it if it is a function identifier.  For NODE,
OVERRIDE, START, END, and ARGS, see `treesit-font-lock-rules'."
  (when (not (equal (treesit-node-type
                     (treesit-node-parent node))
                    "call_expression"))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-variable-use-face override start end)))

(defun c-ts-mode--fontify-DEFUN (node override start end &rest _)
  "Correctly fontify calls to the DEFUN macro in Emacs sources.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'.  The captured NODE is a
call_expression node, where DEFUN is the function.

This function corrects the fontification of the colon in
\"doc:\", and of the parameter list."
  (let* ((parent (treesit-node-parent node))
         ;; ARG-LIST-1 and 2 are like this:
         ;;
         ;; DEFUN (ARG-LIST-1)
         ;; (ARG-LIST-2)
         (arg-list-1 (treesit-node-children
                      (treesit-node-child-by-field-name
                       node "arguments")))
         ;; ARG-LIST-2 is the
         (arg-list-2 (treesit-node-children
                      (treesit-node-child-by-field-name
                       parent "arguments") t)))
    ;; Fix the colon.
    (dolist (node arg-list-1)
      (when (equal (treesit-node-text node t) ":")
        (treesit-fontify-with-override
         (treesit-node-start node) (treesit-node-end node)
         'default override start end)))
    ;; Fix the parameter list.
    (while arg-list-2
      (let ((type (and arg-list-2 (pop arg-list-2)))
            (arg (and arg-list-2 (pop arg-list-2))))
        (when type
          (treesit-fontify-with-override
           (treesit-node-start type) (treesit-node-end type)
           'font-lock-type-face override start end))
        (when arg
          (treesit-fontify-with-override
           (treesit-node-start arg) (treesit-node-end arg)
           'default override start end))))))

(defun c-ts-mode--fontify-for-each-tail (node override start end &rest _)
  "Fontify FOR_EACH_* macro variants in Emacs sources.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'.  The captured NODE is a
function_definition node."
  (let ((for-each-tail (treesit-node-child-by-field-name node "type"))
        (args (treesit-node-child-by-field-name node "declarator")))
    (treesit-fontify-with-override
     (treesit-node-start for-each-tail) (treesit-node-end for-each-tail)
     'default override start end)
    (treesit-fontify-with-override
     (1+ (treesit-node-start args)) (1- (treesit-node-end args))
     'default override start end)))

(defun c-ts-mode--fontify-error (node override start end &rest _)
  "Fontify the error nodes.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'."
  (let ((parent (treesit-node-parent node))
        (child (treesit-node-child node 0)))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     (cond
      ;; This matches the case MACRO(struct a, b, c)
      ;; where struct is seen as error.
      ((and (equal (treesit-node-type child) "identifier")
            (equal (treesit-node-type parent) "argument_list")
            (member (treesit-node-text child)
                    '("struct" "long" "short" "enum" "union")))
       'font-lock-keyword-face)
      (t 'font-lock-warning-face))
     override start end)))

;;; Imenu

(defun c-ts-mode--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ((or "function_definition" "declaration")
      (c-ts-mode--declarator-identifier
       (treesit-node-child-by-field-name node "declarator")
       t))
     ((or "struct_specifier" "enum_specifier"
          "union_specifier" "class_specifier"
          "namespace_definition"
          "preproc_def" "preproc_function_def")
      (treesit-node-child-by-field-name node "name"))
     ;; DEFUNs in Emacs sources.
     ("expression_statement"
      (let* ((call-exp-1 (treesit-node-child node 0))
             (call-exp-2 (treesit-node-child call-exp-1 0))
             (arg-list (treesit-node-child call-exp-2 1))
             (name (treesit-node-child arg-list 1 t)))
        name)))
   t))

;;; Outline minor mode

(defun c-ts-mode--outline-predicate (node)
  "Match outlines on lines with function names."
  (or (and (equal (treesit-node-type node) "function_declarator")
           ;; Handle the case when "function_definition" is
           ;; not an immediate parent of "function_declarator"
           ;; but there is e.g. "pointer_declarator" between them.
           (treesit-parent-until node "function_definition"))
      ;; DEFUNs in Emacs sources.
      (and c-ts-mode-emacs-sources-support
           (c-ts-mode--emacs-defun-p node))
      (member (treesit-node-type node) '("namespace_definition"
                                         "class_specifier"))))

;;; Defun navigation

(defun c-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node.
That is, NODE is not nested."
  (let ((top-level-p (lambda (node)
                       (not (treesit-node-top-level
                             node (rx (or "function_definition"
                                          "type_definition"
                                          "struct_specifier"
                                          "enum_specifier"
                                          "union_specifier"
                                          "declaration")))))))
    (pcase (treesit-node-type node)
      ;; The declaration part of a DEFUN.
      ("expression_statement" (c-ts-mode--emacs-defun-p node))
      ;; The body of a DEFUN.
      ("compound_statement" (c-ts-mode--emacs-defun-body-p node))
      ;; If NODE's type is one of these three, make sure it is
      ;; top-level.
      ((or "struct_specifier"
           "enum_specifier"
           "union_specifier")
       (funcall top-level-p node))
      ;; If NODE is a declaration, make sure it's not a function
      ;; declaration (we only want function_definition) and is a
      ;; top-level declaration.
      ("declaration"
       (and (not (equal (treesit-node-type
                         (treesit-node-child-by-field-name
                          node "declarator"))
                        "function_declarator"))
            (funcall top-level-p node)))
      ;; Other types don't need further verification.
      (_ t))))

(defun c-ts-mode--defun-for-class-in-imenu-p (node)
  "Check if NODE is a valid entry for the Class subindex.

Basically, if NODE is a class, return non-nil; if NODE is a
function but is under a class, return non-nil; if NODE is a
top-level function, return nil.

This is for the Class subindex in `treesit-simple-imenu-settings'."
  (pcase (treesit-node-type node)
    ;; The Class subindex only has class_specifier and
    ;; function_definition.
    ("class_specifier" t)
    ("function_definition"
     ;; Return t if this function is nested in a class.
     (treesit-node-top-level node "class_specifier"))))

(defun c-ts-mode--defun-skipper ()
  "Custom defun skipper for `c-ts-mode' and friends.
Structs in C end with a semicolon, but the semicolon is not
considered part of the struct node, so point would stop before
the semicolon.  This function skips the semicolon."
  (when (looking-at (rx (* (or " " "\t")) ";"))
    (goto-char (match-end 0)))
  (treesit-default-defun-skipper))

(defun c-ts-base--before-indent (args)
  (pcase-let ((`(,node ,parent ,bol) args))
    (when (null node)
      (let ((smallest-node (treesit-node-at (point))))
        ;; "Virtual" closer curly added by the
        ;; parser's error recovery.
        (when (and (equal (treesit-node-type smallest-node) "}")
                   (equal (treesit-node-end smallest-node)
                          (treesit-node-start smallest-node)))
          (setq parent (treesit-node-parent smallest-node)))))
    (list node parent bol)))

(defun c-ts-mode--emacs-defun-p (node)
  "Return non-nil if NODE is a Lisp function defined via DEFUN.
This function detects Lisp primitives defined in Emacs source
files using the DEFUN macro."
  (and (equal (treesit-node-type node) "expression_statement")
       (equal (treesit-node-text
               (treesit-node-child-by-field-name
                (treesit-node-child
                 (treesit-node-child node 0) 0)
                "function")
               t)
              "DEFUN")))

(defun c-ts-mode--emacs-defun-body-p (node)
  "Return non-nil if NODE is the function body of a DEFUN."
  (and (equal (treesit-node-type node) "compound_statement")
       (c-ts-mode--emacs-defun-p (treesit-node-prev-sibling node))))

(defun c-ts-mode--emacs-defun-at-point (&optional range)
  "Return the defun node at point.

In addition to regular C functions, this function recognizes
definitions of Lisp primitrives in Emacs source files defined
via DEFUN, if `c-ts-mode-emacs-sources-support' is non-nil.

Note that DEFUN is parsed by tree-sitter as two separate
nodes, one for the declaration and one for the body; this
function returns the declaration node.

If RANGE is non-nil, return (BEG . END) where BEG end END
enclose the whole defun.  This is for when the entire defun
is required, not just the declaration part for DEFUN."
  (when-let* ((node (treesit-defun-at-point))
              (defun-range (cons (treesit-node-start node)
                                 (treesit-node-end node))))
    ;; Make some adjustment for DEFUN.
    (when c-ts-mode-emacs-sources-support
      (cond ((c-ts-mode--emacs-defun-body-p node)
             (setq node (treesit-node-prev-sibling node))
             (setcar defun-range (treesit-node-start node)))
            ((c-ts-mode--emacs-defun-p node)
             (setcdr defun-range (treesit-node-end
                                  (treesit-node-next-sibling node))))))
    (if range defun-range node)))

(defun c-ts-mode-indent-defun ()
  "Indent the current top-level declaration syntactically.

`treesit-defun-type-regexp' defines what constructs to indent."
  (interactive "*")
  (when-let* ((orig-point (point-marker))
              (range (c-ts-mode--emacs-defun-at-point t)))
    (indent-region (car range) (cdr range))
    (goto-char orig-point)))

(defun c-ts-mode--emacs-current-defun-name ()
  "Return the name of the current defun.
This is used for `add-log-current-defun-function'.
In addition to regular C functions, this function also recognizes
Emacs primitives defined via DEFUN in Emacs sources,
if `c-ts-mode-emacs-sources-support' is non-nil."
  (or (treesit-add-log-current-defun)
      (c-ts-mode--defun-name (c-ts-mode--emacs-defun-at-point))))

;;; Things

(defvar c-ts-mode--thing-settings
  `(;; It's more useful to include semicolons as sexp so
    ;; that users can move to the end of a statement.
    (sexp (not (or (and named
                        ,(rx bos (or "translation_unit" "comment") eos))
                   (and anonymous
                        ,(rx (or "{" "}" "[" "]"
                                 "(" ")" ","))))))
    (list
     ,(regexp-opt '("preproc_params"
                    "preproc_if"
                    "preproc_ifdef"
                    "preproc_if_in_field_declaration_list"
                    "preproc_ifdef_in_field_declaration_list"
                    "preproc_if_in_enumerator_list"
                    "preproc_ifdef_in_enumerator_list"
                    "preproc_if_in_enumerator_list_no_comma"
                    "preproc_ifdef_in_enumerator_list_no_comma"
                    "preproc_parenthesized_expression"
                    "preproc_argument_list"
                    "attribute_declaration"
                    "declaration_list"
                    "parenthesized_declarator"
                    "parenthesized_field_declarator"
                    "parenthesized_type_declarator"
                    "abstract_parenthesized_declarator"
                    "compound_statement"
                    "enumerator_list"
                    "field_declaration_list"
                    "parameter_list"
                    "argument_list"
                    "parenthesized_expression"
                    "initializer_list"
                    "subscript_designator"
                    "subscript_range_designator"
                    "string_literal"
                    "system_lib_string"
                    ;; C++
                    "template_parameter_list"
                    "structured_binding_declarator"
                    "template_argument_list"
                    "condition_clause"
                    "subscript_argument_list"
                    "requirement_seq"
                    "requires_parameter_list"
                    "lambda_capture_specifier"
                    "fold_expression")
                  'symbols))
    ;; compound_statement makes us jump over too big units
    ;; of code, so skip that one, and include the other
    ;; statements.
    (sentence
     ,(regexp-opt '("preproc"
                    "declaration"
                    "specifier"
                    "attributed_statement"
                    "labeled_statement"
                    "expression_statement"
                    "if_statement"
                    "switch_statement"
                    "do_statement"
                    "while_statement"
                    "for_statement"
                    "return_statement"
                    "break_statement"
                    "continue_statement"
                    "goto_statement"
                    "case_statement")))
    (text ,(regexp-opt '("comment"
                         "raw_string_literal"))))
  "`treesit-thing-settings' for both C and C++.")

;;; Support for FOR_EACH_* macros
;;
;; FOR_EACH_TAIL, FOR_EACH_TAIL_SAFE, FOR_EACH_FRAME etc., followed by
;; an unbracketed body will mess up the parser, which parses the thing
;; as a function declaration.  We "fix" it by adding a shadow parser
;; with the tag `for-each'.  We use this parser to find each
;; FOR_EACH_* macro with a unbracketed body, and set the ranges of the
;; default C parser so that it skips those FOR_EACH_*'s.  Note that we
;; only ignore FOR_EACH_*'s with a unbracketed body.  Those with a
;; bracketed body parse more or less fine.
;;
;; In the meantime, we have a special fontification rule for
;; FOR_EACH_* macros with a bracketed body that removes any applied
;; fontification (which are wrong anyway), to keep them consistent
;; with the skipped FOR_EACH_* macros (which have no fontification).
;; The rule is in 'emacs-devel' feature.

(defun c-ts-mode--for-each-tail-body-matcher (_n _p bol &rest _)
  "A matcher that matches the first line after a FOR_EACH_* macro.
For BOL see `treesit-simple-indent-rules'."
  (when c-ts-mode-emacs-sources-support
    (save-excursion
      (goto-char bol)
      (forward-line -1)
      (skip-chars-forward " \t")
      (looking-at c-ts-mode--for-each-tail-regexp))))

(defvar c-ts-mode--emacs-c-range-query
  (when (treesit-available-p)
    (treesit-query-compile
     'c `(((declaration
            type: (macro_type_specifier
                   name: (identifier) @_name)
            @for-each-tail)
           (:match ,c-ts-mode--for-each-tail-regexp
                   @_name)))))
  "Query that finds a FOR_EACH_* macro with an unbracketed body.")

(defvar-local c-ts-mode--for-each-tail-ranges nil
  "Ranges covering all the FOR_EACH_* macros in the buffer.")

(defun c-ts-mode--reverse-ranges (ranges beg end)
  "Reverse RANGES and return the new ranges between BEG and END.
Positions that were included in RANGES are not in the returned
ranges, and vice versa.

Return nil if RANGES is nil.  This way, passing the returned
ranges to `treesit-parser-set-included-ranges' will make the
parser parse the whole buffer."
  (if (null ranges)
      nil
    (let ((new-ranges nil)
          (prev-end beg))
      (dolist (range ranges)
        (when (< prev-end (car range))
          (push (cons prev-end (car range)) new-ranges))
        (setq prev-end (cdr range)))
      (when (< prev-end end)
        (push (cons prev-end end) new-ranges))
      (nreverse new-ranges))))

(defun c-ts-mode--emacs-set-ranges (beg end)
  "Set ranges for the C parser to skip some FOR_EACH_* macros.
BEG and END are described in `treesit-range-rules'."
  (let* ((c-parser (treesit-parser-create 'c))
         (for-each-parser (treesit-parser-create 'c nil nil 'for-each))
         (old-ranges c-ts-mode--for-each-tail-ranges)
         (new-ranges (treesit-query-range
                      (treesit-parser-root-node for-each-parser)
                      c-ts-mode--emacs-c-range-query beg end))
         (set-ranges (treesit--clip-ranges
                      (treesit--merge-ranges
                       old-ranges new-ranges beg end)
                      (point-min) (point-max)))
         (reversed-ranges (c-ts-mode--reverse-ranges
                           set-ranges (point-min) (point-max))))
    (setq-local c-ts-mode--for-each-tail-ranges set-ranges)
    (treesit-parser-set-included-ranges c-parser reversed-ranges)))

;;; Modes

(defvar-keymap c-ts-base-mode-map
  :doc "Keymap for C and C-like languages with tree-sitter"
  :parent prog-mode-map
  "C-c C-q" #'c-ts-mode-indent-defun
  "C-c ." #'c-ts-mode-set-style
  "C-c C-c" #'comment-region
  "C-c C-k" #'c-ts-mode-toggle-comment-style)

(define-derived-mode c-ts-base-mode prog-mode "C"
  "Major mode for editing C, powered by tree-sitter.

\\{c-ts-base-mode-map}"
  :syntax-table c-ts-mode--syntax-table

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (cons (regexp-opt (append
                                 '("function_definition"
                                   "type_definition"
                                   "struct_specifier"
                                   "enum_specifier"
                                   "union_specifier"
                                   ;; Make sure this doesn't match
                                   ;; storage_class_specifier.
                                   "^class_specifier$"
                                   "namespace_definition"
                                   "preproc_def"
                                   "preproc_function_def")
                                 (and c-ts-mode-emacs-sources-support
                                      '(;; DEFUN.
                                        "expression_statement"
                                        ;; DEFUN body.
                                        "compound_statement"))))
                    #'c-ts-mode--defun-valid-p))
  (setq-local treesit-defun-skipper #'c-ts-mode--defun-skipper)
  (setq-local treesit-defun-name-function #'c-ts-mode--defun-name)

  ;; IMO it makes more sense to define what's NOT sexp, since sexp by
  ;; spirit, especially when used for movement, is like "expression"
  ;; or "syntax unit". --yuan
  (setq-local treesit-thing-settings
              `((c ,@c-ts-mode--thing-settings)
                (cpp ,@c-ts-mode--thing-settings)))


  ;; When the code is in incomplete state, try to make a better guess
  ;; about which node to indent against.
  (add-function :filter-args (local 'treesit-indent-function)
                #'c-ts-base--before-indent)

  ;; Indent.
  (when (eq c-ts-mode-indent-style 'linux)
    (setq-local indent-tabs-mode t))
  (setq-local c-ts-common-indent-offset 'c-ts-mode-indent-offset)
  ;; This setup is not needed anymore, but we might find uses for it
  ;; later, so I'm keeping it.
  (setq-local c-ts-common-indent-type-regexp-alist
              `((block . ,(rx (or "compound_statement"
                                  "field_declaration_list"
                                  "enumerator_list"
                                  "initializer_list"
                                  "declaration_list")))
                (if . "if_statement")
                (else . ("if_statement" . "alternative"))
                (do . "do_statement")
                (while . "while_statement")
                (for . "for_statement")
                (close-bracket . "}")))
  ;; Comment
  (c-ts-common-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;,#" electric-indent-chars))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              (let ((pred #'c-ts-mode--defun-valid-p))
                `(("Enum" "\\`enum_specifier\\'" ,pred nil)
                  ("Struct" "\\`struct_specifier\\'" ,pred nil)
                  ("Union" "\\`union_specifier\\'" ,pred nil)
                  ("Variable" ,(rx bos "declaration" eos) ,pred nil)
                  ("Function" "\\`function_definition\\'" ,pred nil)
                  ("Class" ,(rx bos (or "class_specifier"
                                        "function_definition")
                                eos)
                   c-ts-mode--defun-for-class-in-imenu-p nil))))

  ;; Outline minor mode
  (setq-local treesit-outline-predicate
              #'c-ts-mode--outline-predicate)

  (setq-local treesit-font-lock-feature-list
              c-ts-mode--feature-list))

(defvar treesit-load-name-override-list)

;;;###autoload
(define-derived-mode c-ts-mode c-ts-base-mode "C"
  "Major mode for editing C, powered by tree-sitter.

This mode is independent from the classic cc-mode.el based
`c-mode', so configuration variables of that mode, like
`c-basic-offset', don't affect this mode.

To use tree-sitter C/C++ modes by default, evaluate

    (add-to-list \\='major-mode-remap-alist \\='(c-mode . c-ts-mode))
    (add-to-list \\='major-mode-remap-alist \\='(c++-mode . c++-ts-mode))
    (add-to-list \\='major-mode-remap-alist
                 \\='(c-or-c++-mode . c-or-c++-ts-mode))

in your init files, or customize `treesit-enabled-modes'."
  :group 'c
  :after-hook (c-ts-mode-set-modeline)

  (when (treesit-ensure-installed 'c)
    ;; Create an "for-each" parser, see `c-ts-mode--emacs-set-ranges'
    ;; for more.
    (when c-ts-mode-emacs-sources-support
      (setq-local treesit-primary-parser
                  (treesit-parser-create 'c nil nil 'for-each)))

    (let ((primary-parser (treesit-parser-create 'c)))
      ;; Comments.
      (setq-local comment-start "/* ")
      (setq-local comment-end " */")
      ;; Indent.
      (setq-local treesit-simple-indent-rules
                  (if (functionp c-ts-mode-indent-style)
                      (funcall c-ts-mode-indent-style)
                    (c-ts-mode--simple-indent-rules
                     'c c-ts-mode-indent-style)))
      ;; (setq-local treesit-simple-indent-rules
      ;;             `((c . ,(alist-get 'gnu (c-ts-mode--indent-styles 'c)))))
      ;; Font-lock.
      (setq-local treesit-font-lock-settings
                  (c-ts-mode--font-lock-settings 'c))
      ;; Navigation.
      (setq-local treesit-defun-tactic 'top-level)
      (treesit-major-mode-setup)

      ;; Emacs source support: handle DEFUN and FOR_EACH_* gracefully.
      (when c-ts-mode-emacs-sources-support
        (setq-local add-log-current-defun-function
                    #'c-ts-mode--emacs-current-defun-name)

        (setq-local treesit-range-settings
                    (treesit-range-rules 'c-ts-mode--emacs-set-ranges))

        (treesit-font-lock-recompute-features '(emacs-devel)))

      ;; Inject doxygen parser for comment.
      (when (and c-ts-mode-enable-doxygen
                 (treesit-ensure-installed 'doxygen))
        (setq-local treesit-primary-parser primary-parser)
        (setq-local treesit-font-lock-settings
                    (append
                     treesit-font-lock-settings
                     c-ts-mode-doxygen-comment-font-lock-settings))
        (setq-local treesit-range-settings
                    (append treesit-range-settings
                            (treesit-range-rules
                             :embed 'doxygen
                             :host 'c
                             :local t
                             `(((comment) @cap
                                (:match
                                 ,c-ts-mode--doxygen-comment-regex @cap))))))))))

(derived-mode-add-parents 'c-ts-mode '(c-mode))

;;;###autoload
(define-derived-mode c++-ts-mode c-ts-base-mode "C++"
  "Major mode for editing C++, powered by tree-sitter.

This mode is independent from the classic cc-mode.el based
`c++-mode', so configuration variables of that mode, like
`c-basic-offset', don't affect this mode.

To use tree-sitter C/C++ modes by default, evaluate

    (add-to-list \\='major-mode-remap-alist \\='(c-mode . c-ts-mode))
    (add-to-list \\='major-mode-remap-alist \\='(c++-mode . c++-ts-mode))
    (add-to-list \\='major-mode-remap-alist
                 \\='(c-or-c++-mode . c-or-c++-ts-mode))

in your init files, or customize `treesit-enabled-modes'.

Since this mode uses a parser, unbalanced brackets might cause
some breakage in indentation/fontification.  Therefore, it's
recommended to enable `electric-pair-mode' with this mode."
  :group 'c++
  :after-hook (c-ts-mode-set-modeline)

  (when (treesit-ensure-installed 'cpp)
    (let ((primary-parser (treesit-parser-create 'cpp)))

      ;; Syntax.
      (setq-local syntax-propertize-function
                  #'c-ts-mode--syntax-propertize)

      ;; Indent.
      (setq-local treesit-simple-indent-rules
                  (if (functionp c-ts-mode-indent-style)
                      (funcall c-ts-mode-indent-style)
                    (c-ts-mode--simple-indent-rules
                     'cpp c-ts-mode-indent-style)))

      ;; Font-lock.
      (setq-local treesit-font-lock-settings
                  (c-ts-mode--font-lock-settings 'cpp))
      (treesit-major-mode-setup)

      (when c-ts-mode-emacs-sources-support
        (setq-local add-log-current-defun-function
                    #'c-ts-mode--emacs-current-defun-name))

      ;; Inject doxygen parser for comment.
      (when (and c-ts-mode-enable-doxygen
                 (treesit-ensure-installed 'doxygen))
        (setq-local treesit-primary-parser primary-parser)
        (setq-local treesit-font-lock-settings
                    (append
                     treesit-font-lock-settings
                     c-ts-mode-doxygen-comment-font-lock-settings))
        (setq-local treesit-range-settings
                    (treesit-range-rules
                     :embed 'doxygen
                     :host 'cpp
                     :local t
                     `(((comment) @cap
                        (:match
                         ,c-ts-mode--doxygen-comment-regex @cap)))))))))

(derived-mode-add-parents 'c++-ts-mode '(c++-mode))

(easy-menu-define c-ts-mode-menu (list c-ts-mode-map c++-ts-mode-map)
  "Menu for `c-ts-mode' and `c++-ts-mode'."
  '("C/C++"
    ["Comment Out Region" comment-region
     :enable mark-active
     :help "Comment out the region between the mark and point"]
    ["Uncomment Region" (comment-region (region-beginning)
                                        (region-end) '(4))
     :enable mark-active
     :help "Uncomment the region between the mark and point"]
    ["Indent Top-level Expression" c-ts-mode-indent-defun
     :help "Indent/reindent top-level function, class, etc."]
    ["Indent Line or Region" indent-for-tab-command
     :help "Indent current line or region, or insert a tab"]
    ["Forward Expression" forward-sexp
     :help "Move forward across one balanced expression"]
    ["Backward Expression" backward-sexp
     :help "Move back across one balanced expression"]
    "--"
    ("Style..."
     ["Set Indentation Style..." c-ts-mode-set-style
      :help "Set C/C++ indentation style for current buffer"]
     ["Show Current Indentation Style" (message "Indentation Style: %s"
                                                c-ts-mode-indent-style)
      :help "Show the name of the C/C++ indentation style for current buffer"]
     ["Set Comment Style" c-ts-mode-toggle-comment-style
      :help "Toggle C/C++ comment style between block and line comments"])
    "--"
    ("Toggle..."
     ["Subword Mode" subword-mode
      :style toggle :selected (bound-and-true-p subword-mode)
      :help "Toggle subword movement and editing mode"])))

;; We could alternatively use parsers, but if this works well, I don't
;; see the need to change.  This is copied verbatim from cc-guess.el.
(defconst c-ts-mode--c-or-c++-regexp
  (eval-when-compile
    (let ((id "[a-zA-Z_][a-zA-Z0-9_]*") (ws "[ \t]+") (ws-maybe "[ \t]*")
          (headers '("string" "string_view" "iostream" "map" "unordered_map"
                     "set" "unordered_set" "vector" "tuple")))
      (concat "^" ws-maybe "\\(?:"
              "using"     ws "\\(?:namespace" ws
              "\\|" id "::"
              "\\|" id ws-maybe "=\\)"
              "\\|" "\\(?:inline" ws "\\)?namespace"
              "\\(?:" ws "\\(?:" id "::\\)*" id "\\)?" ws-maybe "{"
              "\\|" "class"     ws id
              "\\(?:" ws "final" "\\)?" ws-maybe "[:{;\n]"
              "\\|" "struct"     ws id "\\(?:" ws "final" ws-maybe "[:{\n]"
              "\\|" ws-maybe ":\\)"
              "\\|" "template"  ws-maybe "<.*?>"
              "\\|" "#include"  ws-maybe "<" (regexp-opt headers) ">"
              "\\)")))
  "A regexp applied to C header files to check if they are really C++.")

;;;###autoload
(defun c-or-c++-ts-mode ()
  "Analyze buffer and enable either C or C++ mode.

Some people and projects use .h extension for C++ header files
which is also the one used for C header files.  This makes
matching on file name insufficient for detecting major mode that
should be used.

This function attempts to use file contents to determine whether
the code is C or C++, and based on that chooses whether to enable
`c-ts-mode' or `c++-ts-mode'."
  (declare (obsolete c-or-c++-mode "30.1"))
  (interactive)
  (let ((mode
         (if (save-excursion
               (save-restriction
                 (save-match-data       ; Why `save-match-data'?
                   (widen)
                   (goto-char (point-min))
                   (re-search-forward c-ts-mode--c-or-c++-regexp nil t))))
             'c++-ts-mode
           'c-ts-mode)))
    (funcall (major-mode-remap mode))))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(c-mode . c-ts-mode))
  (add-to-list 'treesit-major-mode-remap-alist
               '(c++-mode . c++-ts-mode))
  (add-to-list 'treesit-major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

(provide 'c-ts-mode)
(provide 'c++-ts-mode)

;;; c-ts-mode.el ends here
