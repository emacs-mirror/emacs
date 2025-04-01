(defpackage :lem-java-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*java-mode-hook*
           :java-mode))
(in-package :lem-java-mode)

#|
see : https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html
|#

(defvar *java-keywords*
  '("abstract"   "continue"   "for"          "new"         "switch"
    "assert"     "default"    "if"           "package"     "synchronized"
    "boolean"    "do"         "goto"         "private"     "this"
    "break"      "double"     "implements"   "protected"   "throw"
    "byte"       "else"       "import"       "public"      "throws"
    "case"       "enum"       "instanceof"   "return"      "transient"
    "catch"      "extends"    "int"          "short"       "try"
    "char"       "final"      "interface"    "static"      "void"
    "class"      "finally"    "long"         "strictfp"    "volatile"
    "const"      "float"      "native"       "super"       "while"))


(defvar *java-boolean-literals*
  '("false" "true"))

(defvar *java-null-literal*
  '("null"))

(defvar *java-operators*
  '("="   ">"   "<"   "!"   "~"   "?"   ":"   "->"
    "=="  ">="  "<="  "!="  "&&"  "||"  "++"  "--"
    "+"   "-"   "*"   "/"   "&"   "|"   "^"   "%"   "<<"   ">>"   ">>>"
    "+="  "-="  "*="  "/="  "&="  "|="  "^="  "%="  "<<="  ">>="  ">>>="))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-java ()
  (let ((patterns
          (make-tm-patterns
           (make-tm-line-comment-region "//")
           (make-tm-block-comment-region "/\\*" "\\*/")
           (make-tm-string-region "\'")
           (make-tm-string-region "\"")
           (make-tm-match (tokens :word-boundary *java-keywords*)
                          :name 'syntax-keyword-attribute)
           (make-tm-match (tokens :word-boundary (append *java-boolean-literals*
                                                         *java-null-literal*))
                          :name 'syntax-constant-attribute)
           (make-tm-match (tokens nil *java-operators*)
                          :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *java-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :line-comment-string "//"))
        (tmlanguage (make-tmlanguage-java)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode java-mode language-mode
    (:name "Java"
     :keymap *java-mode-keymap*
     :syntax-table *java-syntax-table*
     :mode-hook *java-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'calc-indent-function) 'lem-c-mode::calc-indent
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "//"))

(define-file-type ("java") java-mode)
