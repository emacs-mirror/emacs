(defpackage :lem-nim-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*nim-mode-hook*
           :nim-mode)
  #+sbcl
  (:lock t))
(in-package :lem-nim-mode)

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings)
                #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

;; numerical literals
;; cf. https://nim-lang.org/docs/manual.html
(let* ((digit "[0-9]")
       (octdigit "[0-7]")
       (hexdigit "[0-9A-Fa-f]")
       (bindigit "[0-1]")
       (hexlit (format nil "0(x|X)~a(_?~a)*"
                      hexdigit hexdigit))
       (declit (format nil "~a(_?~a)*" digit digit))
       (octlit (format nil "0(o|c|C)~a(_?~a)*"
                          octdigit octdigit))
       (binlit (format nil "0(b|b)~a(_?~a)*"
                        bindigit bindigit)))

  (defun integer-literals ()
    (let* ((intlit (format nil "(~@{~a~^|~})"
                       hexlit declit octlit binlit))
           (intsuffix "'?(i|I)(8|16|32|64)"))
      (format nil "\\b~a(~a)?\\b" intlit intsuffix)))

  (defun float-literals ()
    (let* ((exponent (format nil "(e|E)[+-]?~a(_?~a)*"
                         digit digit))
           (floatlit (format nil
                       "~a(_?~a)*((\\.(_?~a)*(~a)?)|~a)"
                         digit digit digit exponent exponent))
           (floatsuffix "((f|F)(32)?|((f|F)64)|d|D)")
           (floatsuffixlit
                  (format nil
                        "(~a'~a)|((~a|~a|~a|~a)'?~a)"
                        hexlit floatsuffix
                        floatlit declit octlit
                        binlit floatsuffix)))
      (format nil "\\b(~a|~a)\\b" floatlit
                  floatsuffixlit))))

;; cf. https://nim-lang.org/docs/manual.html
;; Section: Identifiers & Keywords
(defparameter keywords
 '( "addr" "and" "as" "asm"
    "bind" "block" "break"
    "case" "cast" "concept" "const" "continue" "converter"
    "defer" "discard" "distinct" "div" "do"
    "elif" "else" "end" "enum"
    "except" "export"
    "finally" "for" "from" "func"
    "if" "import" "in" "include" "interface"
        "is" "isnot" "iterator"
    "let" "macro" "method" "mixin" "mod"
    "nil" "not" "notin"
    "object" "of" "or" "out"
    "proc" "ptr" "raise" "ref" "return"
    "shl" "shr" "static"
    "template" "try" "tuple" "type"
    "using" "var" "when" "while" "xor" "yield"
    "result" "echo" "writeLine"))


(defparameter nimtypes
  '(  "int" "int8" "int16" "int32" "int64"
      "uint" "uint8" "uint16" "uint32" "uint64"
      "float" "float32" "float64"
      "bool" "char" "string" "pointer" "typedesc"
      "void" "auto" "any" "sink" "lent"
      "untyped" "typed"  "typedesc"
      "range" "array" "openArray"
      "varargs" "seq" "set" "byte"
      ;; c interop types
      "cchar" "cschar" "cshort" "cint" "clong"
      "clonglong" "cfloat" "cdouble"
      "cstring" "clongdouble" "cstringArray"))

;; cf. https://nim-lang.org/docs/manual.html
(defun make-tmlanguage-nim ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "#" "$" :name
                          'syntax-comment-attribute)
     ;; keywords: https://nim-lang.org/docs/manual.html
     (make-tm-match (tokens :word-boundary keywords)
                :name 'syntax-keyword-attribute)
     (make-tm-match (tokens :word-boundary  nimtypes )
                        :name 'syntax-type-attribute)
     ;; operators: https://nim-lang.org/docs/manual.html
     (make-tm-match (tokens nil
                      '( "=" "+" "-" "*" "/"
                         "<" ">@" "$" "~" "&"
                         "%" "|!" "?" "^" "." ":"))
                     :name 'syntax-keyword-attribute)
     (make-tm-match (tokens :word-boundary
                        '("false" "true" "nil"))
                :name 'syntax-constant-attribute)
     (make-tm-string-region "\"")
     (make-tm-string-region "\"\"\"")
     (make-tm-match (integer-literals)
                :name 'syntax-constant-attribute)
     (make-tm-match (float-literals)
                :name 'syntax-constant-attribute)
     (make-tm-region `(:sequence "#[") `(:sequence "]#")
                     :name 'syntax-comment-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *nim-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\""))
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-nim)))
    (set-syntax-parser table tmlanguage)
    table))

;; three functions below from lem-python-mode,
;; it seems almost works
(defun nim-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(defun beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w")))

(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (unless (search-forward-regexp p "^\\w") (return)))
    (line-start p)
    (move-point point p)))

(define-major-mode nim-mode language-mode
    (:name "nim"
     :keymap *nim-mode-keymap*
     :syntax-table *nim-syntax-table*
     :mode-hook *nim-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'nim-calc-indent
        (variable-value 'line-comment) "#"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(define-file-type ("nim" "nimble") nim-mode)
