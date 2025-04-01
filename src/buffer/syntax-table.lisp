(defpackage :lem/buffer/syntax-table
  (:use :cl)
  (:export :make-syntax-table
           :syntax-table-space-chars
           :syntax-table-symbol-chars
           :syntax-table-paren-pairs
           :syntax-table-string-quote-chars
           :syntax-table-escape-chars
           :syntax-table-fence-chars
           :syntax-table-expr-prefix-chars
           :syntax-table-expr-suffix-chars
           :syntax-table-expr-prefix-forward-function
           :syntax-table-expr-prefix-backward-function
           :syntax-table-line-comment-string
           :syntax-table-block-comment-pairs
           :syntax-table-block-string-pairs
           :syntax-table-parser
           :set-syntax-parser
           :fundamental-syntax-table))
(in-package :lem/buffer/syntax-table)

(defstruct syntax-table
  (space-chars '(#\space #\tab #\newline))
  (symbol-chars '(#\_))
  (paren-pairs '((#\( . #\))
                 (#\[ . #\])
                 (#\{ . #\})))
  (string-quote-chars '(#\"))
  (escape-chars '(#\\))
  fence-chars
  expr-prefix-chars
  expr-suffix-chars
  expr-prefix-forward-function
  expr-prefix-backward-function
  line-comment-string
  block-comment-pairs
  block-string-pairs
  parser)

(defun set-syntax-parser (syntax-table parser)
  (setf (syntax-table-parser syntax-table) parser))

(defparameter *fundamental-syntax-table* (make-syntax-table))

(defun fundamental-syntax-table ()
  *fundamental-syntax-table*)
