;;; csharp-mode.el --- Support for editing C#  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;;              Jostein Kjønigsen <jostein@kjonigsen.net>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;;              Jostein Kjønigsen <jostein@kjonigsen.net>
;; Created    : September 2022
;; Keywords   : c# languages oop

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

;; Support for editing C#.

;;; Code:

(require 'compile)
(require 'cc-mode)
(require 'cc-langs)
(require 'treesit)
(require 'c-ts-common) ; For comment indenting and filling.
(treesit-declare-unavailable-functions)

(eval-when-compile
  (require 'cc-fonts)
  (require 'rx))

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)

(eval-and-compile
  (defconst csharp--regex-identifier
    "[A-Za-z][A-Za-z0-9_]*"
    "Regex describing an identifier in C#.")

  (defconst csharp--regex-identifier-matcher
    (concat "\\(" csharp--regex-identifier "\\)")
    "Regex matching an identifier in C#.")

  (defconst csharp--regex-type-name
    "[A-Z][A-Za-z0-9_]*"
    "Regex describing a type identifier in C#.")

  (defconst csharp--regex-type-name-matcher
    (concat "\\(" csharp--regex-type-name "\\)")
    "Regex matching a type identifier in C#.")

  (defconst csharp--regex-using-or-namespace
    (concat "^using" "\\|" "namespace"
            "\\s *"
            csharp--regex-type-name-matcher)
    "Regex matching identifiers after a using or namespace
    declaration."))

(eval-and-compile
  (c-add-language 'csharp-mode 'java-mode)

  (defun csharp--make-mode-syntax-table ()
    (let ((table (make-syntax-table)))
      (c-populate-syntax-table table)
      (modify-syntax-entry ?@ "_" table)
      table))
  (defvar csharp--make-mode-syntax-table #'csharp--make-mode-syntax-table
    "Workaround for Emacs bug#57065."))

(c-lang-defconst c-make-mode-syntax-table
  csharp #'csharp--make-mode-syntax-table)

(c-lang-defconst c-identifier-syntax-modifications
  csharp (append '((?@ . "w"))
                 (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-start
  csharp (concat "[" c-alpha "_@]"))

(c-lang-defconst c-opt-type-suffix-key
  csharp (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\?\\)"))

(c-lang-defconst c-identifier-ops
  csharp '((left-assoc ".")))

(c-lang-defconst c-overloadable-operators
  csharp '("+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" "=="
           "!=" ">" "<" ">=" "<="))

(c-lang-defconst c-multiline-string-start-char
  csharp ?@)

(c-lang-defconst c-ml-string-opener-re
  ;; "\\(\\(?:@\\$?\\)\\(\"\\)\\)"
  csharp
  (rx
   (group
    (or "@" "@$")
    (group "\""))))

(c-lang-defconst c-ml-string-max-opener-len
  csharp 3)

(c-lang-defconst c-ml-string-max-closer-len
  csharp 2)

(c-lang-defconst c-ml-string-any-closer-re
  ;; "\\(?:\"\"\\)*\\(\\(\"\\)\\)\\(?:[^\"]\\|\\'\\)"
  csharp
  (rx
   (seq
    (zero-or-more "\"\"")
    (group
     (group "\""))
    (or (not (any "\"")) eos))))

(c-lang-defconst c-ml-string-back-closer-re
  ;; "\\(?:\\`\\|[^\"]\\)\"*"
  csharp
  (rx
   (seq
    (or bos
        (not (any "\"")))
    (zero-or-more "\""))))

(c-lang-defconst c-type-prefix-kwds
  csharp '("class" "interface" "struct"))

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "interface" "struct"))

;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  csharp '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
           "long" "ulong" "short" "ushort" "void" "object" "string" "var"))

(c-lang-defconst c-other-decl-kwds
  csharp nil)

(c-lang-defconst c-type-list-kwds
  csharp nil)

(c-lang-defconst c-other-block-decl-kwds
  csharp nil)

(c-lang-defconst c-return-kwds
  csharp '("return"))

(c-lang-defconst c-typedef-kwds
  csharp nil)

(c-lang-defconst c-typeof-kwds
  csharp '("typeof" "is" "as"))

(c-lang-defconst c-type-modifier-prefix-kwds
  csharp '("volatile"))

(c-lang-defconst c-type-modifier-kwds
  csharp '("readonly" "new"))

(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))

(c-lang-defconst c-recognize-post-brace-list-type-p
  csharp t)

(c-lang-defconst c-ref-list-kwds
  csharp nil)

(c-lang-defconst c-using-kwds
  csharp '("using"))

(c-lang-defconst c-equals-type-clause-kwds
  csharp '("using"))

(defun csharp-at-vsemi-p (&optional pos)
  (if pos (goto-char pos))
  (save-excursion
    (beginning-of-line)
    (c-forward-syntactic-ws)
    (looking-at "using\\s *(")))

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp-at-vsemi-p)

(defun csharp-vsemi-status-unknown-p () t)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  csharp 'csharp-vsemi-status-unknown-p)

(c-lang-defconst c-modifier-kwds
  csharp '("abstract" "default" "final" "native" "private" "protected"
           "public" "partial" "internal" "readonly" "static" "event" "transient"
           "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
           "fixed" "override" "params" "async" "await" "extern" "unsafe"
           "get" "set" "this" "const" "delegate"))

(c-lang-defconst c-other-kwds
  csharp '("select" "from" "where" "join" "in" "on" "equals" "into"
           "orderby" "ascending" "descending" "group" "when"
           "let" "by" "namespace"))

(c-lang-defconst c-colon-type-list-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-1-2-kwds
  csharp '("try"))

(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "fixed" "checked"
           "unchecked" "using" "lock"))

(c-lang-defconst c-simple-stmt-kwds
  csharp '("break" "continue" "goto" "throw" "return" "yield"))

(c-lang-defconst c-constant-kwds
  csharp  '("true" "false" "null" "value"))

(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base" "operator"))

(c-lang-defconst c-inexpr-class-kwds
  csharp nil)

(c-lang-defconst c-std-abbrev-keywords
  csharp (append (c-lang-const c-std-abbrev-keywords) '("catch" "finally")))

(c-lang-defconst c-decl-prefix-re
  csharp "\\([{}(;,<]+\\)")

(c-lang-defconst c-recognize-typeless-decls
  csharp t)

(c-lang-defconst c-recognize-<>-arglists
  csharp t)

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")

(c-lang-defconst c-opt-cpp-macro-define
  csharp (if (c-lang-const c-opt-cpp-prefix)
             "define"))

(c-lang-defconst c-cpp-message-directives
  csharp '("error" "warning" "region"))

(c-lang-defconst c-cpp-expr-directives
  csharp '("if" "elif"))

(c-lang-defconst c-other-op-syntax-tokens
  csharp  (append '("#")
                  (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-line-comment-starter
  csharp "//")

(c-lang-defconst c-doc-comment-start-regexp
  csharp "///")

(c-add-style "csharp"
             '("java"
               (c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((inline-open           . 0)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   (inexpr-class          . 0)
                                   (case-label            . +)
                                   (cpp-macro             . c-lineup-dont-change)
                                   (substatement-open     . 0)))))

(eval-and-compile
  (unless (or (stringp c-default-style)
              (assoc 'csharp-mode c-default-style))
    (setq c-default-style
          (cons '(csharp-mode . "csharp")
                c-default-style))))

(defun csharp--color-forwards (font-lock-face)
  (let (id-beginning)
    (goto-char (match-beginning 0))
    (forward-word)
    (while (and (not (or (eq (char-after) ?\;)
                         (eq (char-after) ?\{)))
                (progn
                  (forward-char)
                  (c-forward-syntactic-ws)
                  (setq id-beginning (point))
                  (> (skip-chars-forward
                      (c-lang-const c-symbol-chars))
                     0))
                (not (get-text-property (point) 'face)))
      (c-put-font-lock-face id-beginning (point) font-lock-face)
      (c-forward-syntactic-ws))))

(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;; Warning face on unclosed strings
           ("\\s|" 0 font-lock-warning-face t nil)

           ;; Invalid single quotes
           c-font-lock-invalid-single-quotes

           ;; Keyword constants
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))

           ;; Keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)

           ;; Chained identifiers in using/namespace statements
           ,`(,(c-make-font-lock-search-function
                csharp--regex-using-or-namespace
                `((csharp--color-forwards 'font-lock-variable-name-face)
                  nil
                  (goto-char (match-end 0)))))

           ;; Negation character
           (eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))

           ;; Types after 'new'
           (eval . (list (concat "\\<new\\> *" csharp--regex-type-name-matcher)
                         1 font-lock-type-face))

           ;; Single identifier in attribute
           (eval . (list (concat "\\[" csharp--regex-type-name-matcher "\\][^;]")
                         1 'font-lock-variable-name-face t))

           ;; Function names
           (eval . (list "\\([A-Za-z0-9_]+\\)\\(<[a-zA-Z0-9, ]+>\\)?("
                         1 font-lock-function-name-face))

           ;; Nameof
           (eval . (list (concat "\\(\\<nameof\\>\\) *(")
                         1 font-lock-function-name-face))

           (eval . (list (concat "\\<nameof\\> *( *"
                                 csharp--regex-identifier-matcher
                                 " *) *")
                         1 'font-lock-variable-name-face))

           ;; Catch statements with type only
           (eval . (list (concat "\\<catch\\> *( *"
                                 csharp--regex-type-name-matcher
                                 " *) *")
                         1 font-lock-type-face))))

(c-lang-defconst c-basic-matchers-after
  csharp (append
          ;; Merge with cc-mode defaults - enables us to add more later
          (c-lang-const c-basic-matchers-after)))

(defcustom csharp-codedoc-tag-face 'c-doc-markup-face-name
  "Face to be used on the codedoc docstring tags.

Should be one of the font lock faces, such as
`font-lock-variable-name-face' and friends.

Needs to be set before `csharp-mode' is loaded, because of
compilation and evaluation time conflicts."
  :type 'symbol)

(defcustom csharp-font-lock-extra-types
  (list csharp--regex-type-name)
  (c-make-font-lock-extra-types-blurb "C#" "csharp-mode" (concat))
  :type 'c-extra-types-widget
  :group 'c)

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal font locking for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal font locking for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal font locking for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defun csharp-font-lock-keywords-2 ()
  (c-compose-keywords-list csharp-font-lock-keywords-2))
(defun csharp-font-lock-keywords-3 ()
  (c-compose-keywords-list csharp-font-lock-keywords-3))
(defun csharp-font-lock-keywords ()
  (c-compose-keywords-list csharp-font-lock-keywords))

;;; Doc comments

(defconst codedoc-font-lock-doc-comments
  ;; Most of this is taken from the javadoc example, however, we don't use the
  ;; '@foo' syntax, so I removed that. Supports the XML tags only
  `((,(concat "</?\\sw"         ; XML tags.
              "\\("
              (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                      "\"[^\"]*\"\\|'[^']*'")
              "\\)*/?>")
     0 ,csharp-codedoc-tag-face prepend nil)
    ;; ("\\([a-zA-Z0-9_]+\\)=" 0 font-lock-variable-name-face prepend nil)
    ;; ("\".*\"" 0 font-lock-string-face prepend nil)
    ("&\\(\\sw\\|[.:]\\)+;"     ; XML entities.
     0 ,csharp-codedoc-tag-face prepend nil)))

(defconst codedoc-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "///" limit
          codedoc-font-lock-doc-comments)))))

;;; End of doc comments

;;; Adding syntax constructs

(advice-add 'c-looking-at-inexpr-block
            :around #'csharp-looking-at-inexpr-block)

(defun csharp-looking-at-inexpr-block (orig-fun &rest args)
  (let ((res (csharp-at-lambda-header)))
    (if res
        res
      (apply orig-fun args))))

(defun csharp-at-lambda-header ()
  (save-excursion
    (c-backward-syntactic-ws)
    (unless (bobp)
      (backward-char)
      (c-safe (goto-char (scan-sexps (point) -1)))
      (when (or (looking-at "([[:alnum:][:space:]_,]*)[ \t\n]*=>[ \t\n]*{")
                (looking-at "[[:alnum:]_]+[ \t\n]*=>[ \t\n]*{"))
        ;; If we are at a C# lambda header
        (cons 'inexpr (point))))))

(advice-add 'c-guess-basic-syntax
            :around #'csharp-guess-basic-syntax)

(defun csharp-guess-basic-syntax (orig-fun &rest args)
  (cond
   (;; enum
    (save-excursion
      (goto-char (c-point 'boi))
      (and
       (eq (char-after) ?\{)
       (save-excursion
         (goto-char (c-point 'iopl))
         (looking-at ".*enum.*"))))
    `((class-open ,(c-point 'iopl))))
   (;; Attributes
    (save-excursion
      (goto-char (c-point 'iopl))
      (and
       (eq (save-excursion
             (skip-chars-forward " \t\n")
             (char-after))
           ?\[)
       (save-excursion
         (c-go-list-forward)
         (and (eq (char-before) ?\])
              (not (eq (char-after) ?\;))))))
    `((annotation-top-cont ,(c-point 'iopl))))
   ((and
     ;; Heuristics to find object initializers
     (save-excursion
       ;; Next non-whitespace character should be '{'
       (goto-char (c-point 'boi))
       (unless (eq (char-after) ?{)
         (ignore-errors (backward-up-list 1 t t)))
       (save-excursion
         ;; 'new' should be part of the line, but should not trigger if
         ;; statement has already ended, like for 'var x = new X();'.
         ;; Also, deal with the possible end of line obscured by a
         ;; trailing comment.
         (goto-char (c-point 'iopl))
         (when (looking-at-p ".*new.*")
           (if (re-search-forward ";" (pos-eol) t 1)
               ;; If the ';' is inside a comment, the statement hasn't
               ;; likely ended, so we should accept as object init.
               ;; Example:
               ;; var x = new         // This should return true ;
               ;; var x = new();      // This should return false ;
               (nth 4 (syntax-ppss (point)))
             t))))
     ;; Line should not already be terminated
     (save-excursion
       (goto-char (c-point 'eopl))
       (or (not (eq (char-before) ?\;))
           (not (eq (char-before) ?\{)))))
    (cond
     ((save-excursion
        ;; if we have a hanging brace on line before
        (goto-char (c-point 'eopl))
        (eq (char-before) ?\{))
      `((brace-list-intro ,(c-point 'iopl))))
     ((save-excursion
        ;; if we have a hanging brace on line before
        (goto-char (c-point 'boi))
        (and (eq (char-after) ?\})
             `((brace-list-close ,(save-excursion
                                    (backward-up-list 1 t t)
                                    (point)))))))
     (t
      `((block-open) (statement ,(c-point 'iopl))))))
   (t
    (apply orig-fun args))))

;;; End of new syntax constructs

;; When invoked by MSBuild, csc’s errors look like this:
;; subfolder\file.cs(6,18): error CS1006: Name of constructor must
;; match name of class [c:\Users\user\project.csproj]

(defun csharp--compilation-error-file-resolve ()
  "Resolve an msbuild error to a (filename . dirname) cons cell."
  ;; https://stackoverflow.com/a/18049590/429091
  (cons (match-string 1) (file-name-directory (match-string 4))))

(defconst csharp-compilation-re-msbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "error [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation error from msbuild.")

(defconst csharp-compilation-re-msbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "warning [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation warning from msbuild.")

;; Notes on xbuild and devenv commonalities
;;
;; These regexes were tailored for xbuild, but apart from the concurrent
;; build-marker ("1>") they share exactly the same match-markers.
;;
;; If we don't exclude the match-markers explicitly, these regexes
;; will also be used to match for devenv as well, including the build-marker
;; in the file-name, causing the lookup to fail.
;;
;; So if we don't want devenv to fail, we actually need to handle it in our
;; xbuild-regexes, but then we automatically get devenv-support for free.

(defconst csharp-compilation-re-xbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)*): "
   "error [[:alnum:]]+: .+$")
  "Regexp to match compilation error from xbuild.")

(defconst csharp-compilation-re-xbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)*): "
   "warning [[:alnum:]]+: .+$")
  "Regexp to match compilation warning from xbuild.")

(defconst csharp-compilation-re-dotnet-error
  "[[:blank:]]*\\([^\r\n]+\\) : error [A-Z]+[0-9]+:")

(defconst csharp-compilation-re-dotnet-warning
  "[[:blank:]]*\\([^\r\n]+\\) : warning [A-Z]+[0-9]+:")

(defconst csharp-compilation-re-dotnet-testfail
  (concat
   "[[:blank:]]+Stack Trace:\n"
   "[[:blank:]]+at [^\n]+ in \\([^\n]+\\):line \\([0-9]+\\)"))

(eval-after-load 'compile
  (lambda ()
    (dolist
        (regexp
         `((dotnet-testfail
            ,csharp-compilation-re-dotnet-testfail
            1 2)
           (xbuild-error
            ,csharp-compilation-re-xbuild-error
            1 2 3 2)
           (xbuild-warning
            ,csharp-compilation-re-xbuild-warning
            1 2 3 1)
           (msbuild-error
            ,csharp-compilation-re-msbuild-error
            csharp--compilation-error-file-resolve
            2
            3
            2
            nil
            (1 compilation-error-face)
            (4 compilation-error-face))
           (msbuild-warning
            ,csharp-compilation-re-msbuild-warning
            csharp--compilation-error-file-resolve
            2
            3
            1
            nil
            (1 compilation-warning-face)
            (4 compilation-warning-face))
           (dotnet-error
            ,csharp-compilation-re-dotnet-error
            1)
           (dotnet-warning
            ,csharp-compilation-re-dotnet-warning
            1 nil nil 1)))
      (add-to-list 'compilation-error-regexp-alist-alist regexp)
      (add-to-list 'compilation-error-regexp-alist (car regexp)))))

(defvar csharp-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table csharp))
  "Syntax table used in `csharp-mode' buffers.")

(defvar csharp-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in `csharp-mode' buffers.")

(easy-menu-define csharp-mode-menu csharp-mode-map "C# Mode Commands."
  (cons "C#" (c-lang-const c-mode-menu csharp)))

;;; Tree-sitter support

(add-to-list
 'treesit-language-source-alist
 '(c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp"
           :commit "362a8a41b265056592a0c3771664a21d23a71392")
 t)

(defcustom csharp-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `csharp-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'csharp)

(defvar csharp-ts-mode--indent-rules
  `((c-sharp
     ((parent-is "compilation_unit") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "namespace_declaration") parent-bol 0)
     ((parent-is "class_declaration") parent-bol 0)
     ((parent-is "constructor_declaration") parent-bol 0)
     ((parent-is "array_creation_expression") parent-bol 0) ;; actual initialization is in contained initializer_expression.
     ((match "{" "initializer_expression" ) parent-bol 0)
     ((parent-is "initializer_expression") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "anonymous_object_creation_expression") parent-bol 0)
     ((parent-is "anonymous_object_creation_expression") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "object_creation_expression") parent-bol 0)
     ((parent-is "object_creation_expression") parent-bol 0)
     ((parent-is "member_access_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "method_declaration") parent-bol 0)
     ((parent-is "enum_declaration") parent-bol 0)
     ((parent-is "operator_declaration") parent-bol 0)
     ((parent-is "field_declaration") parent-bol 0)
     ((parent-is "struct_declaration") parent-bol 0)
     ((parent-is "declaration_list") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "argument_list") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "interpolation") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent 0)
     ((parent-is "block") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "local_function_statement") parent-bol 0)
     ((match "block" "if_statement") parent-bol 0)
     ((match "else" "if_statement") parent-bol 0)
     ((parent-is "if_statement") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "for_statement") parent-bol 0)
     ((parent-is "for_each_statement") parent-bol 0)
     ((parent-is "while_statement") parent-bol 0)
     ((match "{" "switch_expression") parent-bol 0)
     ((parent-is "switch_statement") parent-bol 0)
     ((parent-is "switch_body") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "switch_section") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "switch_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol 0)
     ((parent-is "do_statement") parent-bol 0)
     ((parent-is "equals_value_clause") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "ternary_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "conditional_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "statement_block") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "type_arguments") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "array") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "formal_parameters") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "template_substitution") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "object_pattern") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "object") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "object_type") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "arrow_function") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "using_statement") parent-bol 0)
     ((parent-is "lambda_expression") parent-bol 0)
     ((parent-is "try_statement") parent-bol 0)
     ((parent-is "catch_filter_clause") parent-bol 0)
     ((parent-is "preproc_if") parent-bol 0)
     ((parent-is "preproc_region") parent-bol 0))))

(defvar csharp-ts-mode--keywords
  '("using" "namespace" "class" "if" "else" "throw" "new" "for"
    "return" "await" "struct" "enum" "switch" "case"
    "default" "typeof" "try" "catch" "finally" "break"
    "foreach" "in" "yield" "get" "set" "when" "as" "out"
    "is" "while" "continue" "this" "ref" "goto" "interface"
    "from" "where" "select" "lock" "base" "record" "init"
    "with" "let" "static" "var" "do" "public" "private"
    "readonly" "unmanaged")
  "C# keywords for tree-sitter font-locking.")

(defgroup csharp-ts-mode-faces nil
  "Font faces."
  :group 'csharp)

(defface csharp-ts-mode-attribute-face
  '((t . (:inherit font-lock-property-use-face)))
  "Font face used for fontification of attributes."
  :group 'csharp-ts-mode-faces)

(defun csharp-ts-mode--test-this-expression ()
  "Return non-nil if (this_expression) is named in csharp grammar."
  (treesit-query-valid-p 'c-sharp "(this_expression)"))

(defun csharp-ts-mode--test-interpolated-string-text ()
  "Return non-nil if (interpolated_string_text) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(interpolated_string_text)"))

(defun csharp-ts-mode--test-string-content ()
  "Return non-nil if (interpolated_string_text) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(string_content)"))

(defun csharp-ts-mode--test-type-constraint ()
  "Return non-nil if (type_constraint) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(type_constraint)"))

(defun csharp-ts-mode--test-type-of-expression ()
  "Return non-nil if (type_of_expression) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(type_of_expression)"))

(defun csharp-ts-mode--test-typeof-expression ()
  "Return non-nil if (type_of_expression) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(typeof_expression)"))

(defun csharp-ts-mode--test-name-equals ()
  "Return non-nil if (name_equals) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(name_equals)"))

(defun csharp-ts-mode--test-if-directive ()
  "Return non-nil if (if_directive) is in the grammar."
  (treesit-query-valid-p 'c-sharp "(if_directive)"))

(defun csharp-ts-mode--test-method-declaration-type-field ()
  "Return non-nil if (method_declaration) has a type field."
  (treesit-query-valid-p 'c-sharp "(method_declaration type: (_))"))

(defvar csharp-ts-mode--type-field
  (if (csharp-ts-mode--test-method-declaration-type-field)
      'type: 'returns:))

(defun csharp-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for `csharp-ts-mode'."
  (treesit-font-lock-rules
   :language 'c-sharp
   :feature 'expression
   '((conditional_expression (identifier) @font-lock-variable-use-face)
     (postfix_unary_expression (identifier)* @font-lock-variable-use-face)
     (initializer_expression (assignment_expression left: (identifier) @font-lock-property-use-face))
     (anonymous_object_creation_expression
      (identifier) @font-lock-property-use-face
      (identifier) @font-lock-variable-use-face)
     (anonymous_object_creation_expression
      (identifier) @font-lock-property-use-face
      [(object_creation_expression)
       (integer_literal)
       (string_literal)
       (binary_expression)
       (invocation_expression)
       (member_access_expression)
       (conditional_expression)])
     (interpolated_string_expression
      (interpolation
       (identifier) @font-lock-variable-use-face))
     (interpolated_string_expression
      (interpolation
       (member_access_expression
        name: (identifier) @font-lock-property-use-face)))
     ((interpolated_string_expression
       (interpolation
        (member_access_expression
         expression: (identifier) @font-lock-variable-use-face)))
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((element_access_expression (identifier) @font-lock-variable-use-face)
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((element_access_expression (identifier) @font-lock-variable-use-face)
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((return_statement (identifier) @font-lock-variable-use-face)
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((return_statement (member_access_expression
                         expression: (identifier) @font-lock-variable-use-face))
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((is_pattern_expression
       expression: (identifier) @font-lock-variable-use-face)
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((is_pattern_expression
       expression: (member_access_expression
                    expression: (identifier) @font-lock-variable-use-face))
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     (is_pattern_expression
      expression: (member_access_expression
                   name: (identifier) @font-lock-property-use-face))
     (is_pattern_expression
      pattern: (constant_pattern (identifier) @font-lock-type-face))
     (is_pattern_expression
      pattern: (constant_pattern (member_access_expression
                                  name: (identifier) @font-lock-type-face)))
     ((binary_expression
       left: (identifier) @font-lock-variable-use-face)
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     ((binary_expression
       right: (identifier) @font-lock-variable-use-face)
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     (assignment_expression
      right: (identifier) @font-lock-variable-use-face)
     (expression_statement ;; capture parent node to NOT shadow variable_declaration.
      (assignment_expression
       left: (identifier) @font-lock-variable-use-face))
     (if_statement condition: (identifier) @font-lock-variable-use-face)

     ;; handle more specific matchers before generalized variable-use fallback.
     (invocation_expression
      function: (member_access_expression
                 name: (identifier) @font-lock-function-call-face))
     (invocation_expression
      function: (member_access_expression
                 name: (generic_name (identifier) @font-lock-function-call-face)))
     (member_access_expression
      expression: (identifier) @font-lock-variable-use-face
      name: (identifier) @font-lock-property-use-face))

   :language 'c-sharp
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}" (interpolation_brace)]) @font-lock-bracket-face)

   :language 'c-sharp
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)

   :language 'c-sharp
   :feature 'error
   '((ERROR) @font-lock-warning-face)

   :language 'c-sharp
   :override t
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'c-sharp
   :override t
   :feature 'keyword
   `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
     (modifier) @font-lock-keyword-face
     ,@(if (csharp-ts-mode--test-this-expression)
           '((this_expression) @font-lock-keyword-face)
         '("this" @font-lock-keyword-face))

     ;; Avoid fontifying identifiers with a keyword-values as identifiers.
     ((identifier) @font-lock-keyword-face
      (:match ,(concat "\\`" (regexp-opt csharp-ts-mode--keywords t) "\\'") @font-lock-keyword-face)))

   :language 'c-sharp
   :override t
   :feature 'attribute
   `((attribute_list
      "[" @csharp-ts-mode-attribute-face
      (attribute name: (identifier) @csharp-ts-mode-attribute-face)
      "]" @csharp-ts-mode-attribute-face))

   :language 'c-sharp
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)

   :language 'c-sharp
   :override t
   :feature 'literal
   `((integer_literal) @font-lock-number-face
     (real_literal) @font-lock-number-face
     (null_literal) @font-lock-constant-face
     (boolean_literal) @font-lock-constant-face)

   :language 'c-sharp
   :feature 'string
   `([(string_literal)
      (verbatim_string_literal)
      ,@ (when (csharp-ts-mode--test-string-content)
           '((string_content)
             "\""))
      ,@(if (csharp-ts-mode--test-interpolated-string-text)
            '((interpolated_string_text)
              (interpolated_verbatim_string_text)
              (character_literal)
              "\""
              "$\""
              "@$\""
              "$@\"")
          '((interpolation_start)
            (interpolation_quote)))]
     @font-lock-string-face)

   :language 'c-sharp
   :feature 'type
   `((predefined_type) @font-lock-type-face
     (implicit_type) @font-lock-type-face
     (nullable_type) @font-lock-type-face
     (type_parameter
      (identifier) @font-lock-type-face)
     (type_argument_list
      (identifier) @font-lock-type-face)
     (type_argument_list
      (generic_name
       (identifier) @font-lock-type-face))
     (base_list
      (generic_name
       (identifier) @font-lock-type-face))
     (array_type
      (identifier) @font-lock-type-face)
     (qualified_name
      name: (generic_name (identifier) @font-lock-type-face))
     (cast_expression (identifier) @font-lock-type-face)
     (cast_expression (generic_name (identifier) @font-lock-type-face))
     ["operator"] @font-lock-type-face
     (type_parameter_constraints_clause
      (identifier) @font-lock-type-face)
     ,@(if (csharp-ts-mode--test-type-constraint)
           '((type_constraint type: (identifier) @font-lock-type-face)
             (type_constraint type: (generic_name (identifier) @font-lock-type-face)))
         '((type_parameter_constraint (type type: (identifier) @font-lock-type-face))
           (type_parameter_constraint (type type: (generic_name (identifier) @font-lock-type-face)))))

     ,@(when (csharp-ts-mode--test-type-of-expression)
         '((type_of_expression (identifier) @font-lock-type-face)))

     ,@(when (csharp-ts-mode--test-typeof-expression)
         '((typeof_expression (identifier) @font-lock-type-face)))

     (object_creation_expression
      type: (identifier) @font-lock-type-face)
     (object_creation_expression
      type: (generic_name (identifier) @font-lock-type-face))
     (as_expression right: (identifier) @font-lock-type-face)
     (as_expression right: (generic_name (identifier) @font-lock-type-face)))

   :language 'c-sharp
   :feature 'definition
   `((qualified_name (identifier) @font-lock-type-face)
     (using_directive (identifier) @font-lock-type-face)
     ,@(when (csharp-ts-mode--test-name-equals)
         '((using_directive (name_equals
                             (identifier) @font-lock-type-face))))

     (enum_declaration (identifier) @font-lock-type-face)
     (enum_member_declaration (identifier) @font-lock-variable-name-face)
     (field_declaration (variable_declaration (variable_declarator
                                               name: (identifier) @font-lock-variable-name-face)))

     (interface_declaration (identifier) @font-lock-type-face)

     (struct_declaration (identifier) @font-lock-type-face)

     (record_declaration (identifier) @font-lock-type-face)
     (namespace_declaration (identifier) @font-lock-type-face)
     (base_list (identifier) @font-lock-type-face)
     (property_declaration
      type: (nullable_type) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (property_declaration
      type: (predefined_type) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (property_declaration
      type: (identifier) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (class_declaration (identifier) @font-lock-type-face)

     (constructor_declaration name: (_) @font-lock-type-face)
     ;; Handle different releases of tree-sitter-c-sharp.
     ;; Check if keyword void_keyword is available, then return the correct rule."
     ,@(condition-case nil
           (progn (treesit-query-capture 'csharp '((void_keyword) @capture))
                  `((method_declaration ,csharp-ts-mode--type-field [(identifier) (void_keyword)] @font-lock-type-face)))
         (error
          `((method_declaration ,csharp-ts-mode--type-field [(identifier) (predefined_type)] @font-lock-type-face))))
     (method_declaration ,csharp-ts-mode--type-field (generic_name (identifier) @font-lock-type-face))
     (method_declaration name: (_) @font-lock-function-name-face)

     ;; only fontify known expression-types, to avoid the need to use :override
     ;; for lambda-expressions in 'function below.
     (variable_declarator
      name: (identifier) @font-lock-variable-name-face
      [(object_creation_expression)
       (integer_literal)
       (string_literal)
       (binary_expression)
       (invocation_expression)
       (await_expression)
       (member_access_expression)
       (conditional_expression)])

     (catch_declaration
      type: (identifier) @font-lock-type-face)
     (catch_declaration
      name: (identifier) @font-lock-variable-name-face)

     (variable_declaration (identifier) @font-lock-type-face)
     (variable_declaration (qualified_name
                            name: (generic_name (identifier) @font-lock-type-face)))
     (variable_declaration (generic_name (identifier) @font-lock-type-face))

     (parameter type: (identifier) @font-lock-type-face)
     (parameter type: (generic_name (identifier) @font-lock-type-face))
     (parameter name: (identifier) @font-lock-variable-name-face)

     (lambda_expression (identifier) @font-lock-variable-name-face)
     (lambda_expression
      parameters: (implicit_parameter) @font-lock-variable-name-face)

     (declaration_expression type: (identifier) @font-lock-type-face)
     (declaration_expression name: (identifier) @font-lock-variable-name-face))

   :language 'c-sharp
   :feature 'function
   '((invocation_expression
      function: (identifier) @font-lock-function-call-face)
     ((invocation_expression
       function: (member_access_expression
                  expression: (identifier) @font-lock-variable-use-face))
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     (argument (identifier) @font-lock-variable-use-face)
     ((argument (member_access_expression
                 expression: (identifier) @font-lock-variable-use-face))
      (:match "^[a-z][A-Za-z0-9]+" @font-lock-variable-use-face))
     (argument (member_access_expression
                name: (identifier) @font-lock-property-use-face))
     ;; only highlight as function if variable contains lambda expression
     (variable_declarator
      name: (identifier) @font-lock-function-name-face
      (lambda_expression)))

   :language 'c-sharp
   :feature 'directives
   :override t
   (if (csharp-ts-mode--test-if-directive)
       '((if_directive
          "if" @font-lock-preprocessor-face
          (identifier) @font-lock-variable-use-face)
         (elif_directive
          "elif" @font-lock-preprocessor-face
          (identifier) @font-lock-variable-use-face)
         (else_directive) @font-lock-preprocessor-face
         (endif_directive) @font-lock-preprocessor-face
         (define_directive
          "define" @font-lock-preprocessor-face
          (identifier) @font-lock-variable-use-face)
         (nullable_directive) @font-lock-preprocessor-face
         (pragma_directive) @font-lock-preprocessor-face
         (region_directive) @font-lock-preprocessor-face
         (endregion_directive) @font-lock-preprocessor-face
         (region_directive
          (preproc_message) @font-lock-variable-use-face)
         (endregion_directive
          (preproc_message) @font-lock-variable-use-face))
     '((preproc_if
        "#if" @font-lock-preprocessor-face
        (identifier) @font-lock-variable-use-face)
       (preproc_elif
        "#elif" @font-lock-preprocessor-face
        (identifier) @font-lock-variable-use-face)
       (preproc_else) @font-lock-preprocessor-face
       "#endif" @font-lock-preprocessor-face
       (preproc_define
        "#define" @font-lock-preprocessor-face
        (preproc_arg) @font-lock-variable-use-face)
       (preproc_nullable) @font-lock-preprocessor-face
       (preproc_pragma) @font-lock-preprocessor-face
       (preproc_region) @font-lock-preprocessor-face
       (preproc_endregion) @font-lock-preprocessor-face
       (preproc_region
        (preproc_arg) @font-lock-variable-use-face)
       (preproc_endregion
        (preproc_arg) @font-lock-variable-use-face)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(defun csharp-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "method_declaration"
         "record_declaration"
         "struct_declaration"
         "enum_declaration"
         "interface_declaration"
         "class_declaration")
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))))

;;;###autoload
(define-derived-mode csharp-mode prog-mode "C#"
  "Major mode for editing Csharp code.

Key bindings:
\\{csharp-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars csharp-mode)
  (c-common-init 'csharp-mode)
  (setq-local c-doc-comment-style '((csharp-mode . codedoc)))
  (run-mode-hooks 'c-mode-common-hook))

;;;###autoload
(define-derived-mode csharp-ts-mode prog-mode "C#"
  "Major mode for editing C# code."
  :syntax-table (csharp--make-mode-syntax-table)

  (unless (treesit-ensure-installed 'c-sharp)
    (error "Tree-sitter for C# isn't available"))

  ;; Tree-sitter.
  (setq treesit-primary-parser (treesit-parser-create 'c-sharp))

  ;; Comments.
  (c-ts-common-comment-setup)

  (setq-local treesit-thing-settings
              `((c-sharp
                 (list
                  ,(rx bos (or "global_attribute"
                               "attribute_argument_list"
                               "attribute_list"
                               "enum_member_declaration_list"
                               "type_parameter_list"
                               "declaration_list"
                               "accessor_list"
                               "bracketed_parameter_list"
                               "parameter_list"
                               "argument_list"
                               "tuple_pattern"
                               "block"
                               "bracketed_argument_list"
                               "type_argument_list"
                               "array_rank_specifier"
                               "function_pointer_type"
                               "tuple_type"
                               "_for_statement_conditions"
                               "switch_body"
                               "catch_declaration"
                               "catch_filter_clause"
                               "parenthesized_pattern"
                               "list_pattern"
                               "positional_pattern_clause"
                               "property_pattern_clause"
                               "parenthesized_variable_designation"
                               "_switch_expression_body"
                               "interpolated_string_expression"
                               "interpolation"
                               "parenthesized_expression"
                               "_parenthesized_lvalue_expression"
                               "anonymous_object_creation_expression"
                               "initializer_expression"
                               "_with_body"
                               "tuple_expression"
                               "preproc_parenthesized_expression")
                       eos))
                 (sentence
                  ,(rx bos (or "extern_alias_directive"
                               "using_directive"
                               "file_scoped_namespace_declaration"
                               "enum_declaration"
                               "delegate_declaration"
                               "_declaration_list_body"
                               "field_declaration"
                               "event_declaration"
                               "event_field_declaration"
                               "indexer_declaration"
                               "property_declaration"
                               "_function_body"
                               "break_statement"
                               "continue_statement"
                               "do_statement"
                               "empty_statement"
                               "expression_statement"
                               "return_statement"
                               "yield_statement"
                               "throw_statement"
                               "goto_statement"
                               "local_declaration_statement")
                       eos))
                 (text
                  ,(regexp-opt '("comment"
                                 "verbatim_string-literal"
                                 "interpolated_verbatim_string-text"))))))

  ;; Indent.
  (setq-local treesit-simple-indent-rules csharp-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "declaration")
  (setq-local treesit-defun-name-function #'csharp-ts-mode--defun-name)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (csharp-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword string type directives)
                ( constant escape-sequence expression literal attribute)
                ( function bracket delimiter error)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Class" "\\`class_declaration\\'" nil nil)
                ("Interface" "\\`interface_declaration\\'" nil nil)
                ("Enum" "\\`enum_declaration\\'" nil nil)
                ("Record" "\\`record_declaration\\'" nil nil)
                ("Struct" "\\`struct_declaration\\'" nil nil)
                ("Method" "\\`method_declaration\\'" nil nil)))

  ;; Outline minor mode.
  (setq-local treesit-outline-predicate
              (rx bos (or "namespace_declaration"
                          "class_declaration"
                          "interface_declaration"
                          "enum_declaration"
                          "record_declaration"
                          "struct_declaration"
                          "method_declaration"
                          "local_function_statement")
                  eos))

  (treesit-major-mode-setup))

(derived-mode-add-parents 'csharp-ts-mode '(csharp-mode))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(csharp-mode . csharp-ts-mode)))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
