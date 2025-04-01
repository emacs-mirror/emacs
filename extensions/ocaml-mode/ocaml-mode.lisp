(defpackage :lem-ocaml-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*ocaml-mode-hook*
           :ocaml-mode))
(in-package :lem-ocaml-mode)

#| http://caml.inria.fr/pub/docs/manual-ocaml/lex.html |#

(defvar *ocaml-integer-literals*
  (concatenate 'string
               "\\b("
               "((-)?[0-9][0-9_]*)"
               "|((-)?0(x|X)[0-9A-Fa-f][0-9A-Fa-f_]*)"
               "|((-)?0(o|O)[0-7][0-7_]*)"
               "|((-)?0(b|B)[0-1][0-1_]*)"
               "[lLn]?"
               ")\\b"))

(defvar *ocaml-floting-point-literals*
  (concatenate 'string
               "\\b("
               "(*-)?[0-9][0-9_]*(\.[0-9_]*)?([eE][+-]?[0-9][0-9_]*)?)"
               "|((-)?0(x|X)[0-9A-Fa-f][0-9A-Fa-f_]*(\.[0-9A-Fa-f_]*)?([pP][+-]?[0-9][0-9_]*)?)"
               ")\\b"))

(defvar *ocaml-keywords*
  '("and" "as" "assert" "asr" "begin" "class"
    "constraint" "do" "done" "downto" "else" "end"
    "exception" "external" #+(or) "false" "for" "fun" "function"
    "functor" "if" "in" "include" "inherit" "initializer"
    "land" "lazy" "let" "lor" "lsl" "lsr"
    "lxor" "match" "method" "mod" "module" "mutable"
    "new" "nonrec" "object" "of" "open" "or"
    "private" "rec" "sig" "struct" "then" "to"
    #+(or)"true" "try" "type" "val" "virtual" "when"
    "while" "with"))

(defvar *ocaml-operators*
  '("!="    "#"     "&"     "&&"    #+(or) "'"     "("     ")"     "*"     "+"     ","     "-"
    "-."    "->"    "."     ".."    ".~"    ":"     "::"    ":="    ":>"    ";"     ";;"
    "<"     "<-"    "="     ">"     ">]"    ">}"    "?"     "["     "[<"    "[>"    "[|"
    "]"     #+(or) "_"     "`"     "{"     "{<"    "|"     "|]"    "||"    "}"     "~"))

(defvar *ocaml-constants*
  '("false" "true"))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-ocaml ()
  (let ((patterns
          (make-tm-patterns
           (make-tm-block-comment-region "(*" "*)")
           (make-tm-string-region "\"")
           ;(make-tm-string-region "\'")
           (make-tm-match *ocaml-integer-literals*
                          :name 'syntax-constant-attribute)
           #+(or)
           (make-tm-match *ocaml-floting-point-literals*
                          :name 'syntax-constant-attribute)
           (make-tm-match (tokens nil *ocaml-operators*)
                          :name 'syntax-builtin-attribute)
           (make-tm-match (tokens :word-boundary *ocaml-keywords*)
                          :name 'syntax-keyword-attribute)
           (make-tm-match (tokens :word-boundary *ocaml-constants*)
                          :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *ocaml-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :block-comment-pairs '(("(*" . "*)"))))
        (tmlanguage (make-tmlanguage-ocaml)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode ocaml-mode language-mode
    (:name "OCaml"
     :keymap *ocaml-mode-keymap*
     :syntax-table *ocaml-syntax-table*
     :mode-hook *ocaml-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2))

(define-file-type ("ml") ocaml-mode)
