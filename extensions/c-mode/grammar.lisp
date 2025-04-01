(defpackage :lem-c-mode.grammar
  (:use :cl :lem)
  (:export :make-tmlanguage-c)
  #+sbcl
  (:lock t))
(in-package :lem-c-mode.grammar)

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-c ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "//" "$" :name 'syntax-comment-attribute)
                    (make-tm-region '(:sequence "/*")
                                    '(:sequence "*/")
                                    :name 'syntax-comment-attribute)
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")))
                    (make-tm-match "'\\\\?.'"
                                   :name 'syntax-string-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("break" "case" "continue" "default"
                                             "do" "else" "for" "goto" "if"
                                             "_Pragma" "return" "switch" "while"))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match
                     (tokens :word-boundary
                             '("asm" "__asm__" "auto" "bool" "_Bool"
                               "char" "_Complex" "double" "enum" "float"
                               "_Imaginary" "int" "long" "short" "signed"
                               "struct" "typedef" "union" "unsigned" "void"))
                     :name 'syntax-type-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("const" "extern" "register" "restrict"
                                             "static" "volatile" "inline"))
                                   :name 'syntax-builtin-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("NULL" "true" "false" "TRUE" "FALSE"))
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens nil '("+" "++" "+=" "-" "--" "-=" "*" "*=" "/" "/=" "%" "%=" 
                                                 "<" "<=" ">" ">=" "!=" "==" 
                                                 "!" "&&" "||"
                                                 "<<" "<<=" ">>" ">>=" "~" "&" "&=" "|" "|=" "^" "^=" 
                                                 "=" "->" "." "," "?" ":" "sizeof"))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match "\\b((0(x|X)[0-9a-fA-F]([0-9a-fA-F']*[0-9a-fA-F])?)|(0(b|B)[01]([01']*[01])?)|(([0-9]([0-9']*[0-9])?\\.?[0-9]*([0-9']*[0-9])?)|(\\.[0-9]([0-9']*[0-9])?))((e|E)(\\+|-)?[0-9]([0-9']*[0-9])?)?)(L|l|UL|ul|u|U|F|f|ll|LL|ull|ULL)?\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match `(:sequence
                                     :start-anchor
                                     (:greedy-repetition 0 nil :whitespace-char-class)
                                     "#"
                                     ,(tokens nil '("defined" "define" "undef" "include"
                                                    "ifdef" "ifndef" "if" "elif" "else" "endif"
                                                    "line" "error" "pragma"))
                                     :word-boundary)
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))
