(defpackage :lem-haskell-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*haskell-mode-hook*
           :haskell-mode))
(in-package :lem-haskell-mode)

;; https://www.haskell.org/onlinereport/haskell2010/

(defvar *haskell-keywords*
  '("case" "class" "data" "default" "deriving" "do" "else"
    "foreign" "if" "import" "in" "infix" "infixl"
    "infixr" "instance" "let" "module" "newtype" "of"
    "then" "type" "where" "_"))

(defvar *haskell-reserved-operators*
  '(".." ":" "::" "=" "\\" "|" "<-" "->" "@" "~" "=>"))

(defvar *haskell-asc-operators*
  '("!" "#" "$" "%" "&" "⋆" "+" "." "/" "<" "=" ">" "?" "@"
    "\\" "^" "|" "-" "~" ":"))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-haskell ()
  (let* ((integer-literals "\\b(([0-9]+)|(0(o|O)[0-7]+)|(0(x|X)[0-9a-fA-F]+))\\b")
         (float-literals "\\b(([0-9]+\\.[0-9]+([eE](\\+|\\-)?[0-9]+)?)|([0-9]+[eE](\\+|\\-)?[0-9]+))\\b")
         (patterns
           (make-tm-patterns
            (make-tm-line-comment-region "--")
            (make-tm-block-comment-region "{-" "-}")
            (make-tm-string-region "\"")
            (make-tm-match (tokens :word-boundary *haskell-keywords*)
                           :name 'syntax-keyword-attribute)
            (make-tm-match (tokens nil (append *haskell-reserved-operators*
                                               *haskell-asc-operators*))
                           :name 'syntax-builtin-attribute)
            (make-tm-match integer-literals
                           :name 'syntax-constant-attribute)
            (make-tm-match float-literals
                           :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *haskell-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")
                :line-comment-string "--"
                :block-comment-pairs '(("{-" . "-}"))))
        (tmlanguage (make-tmlanguage-haskell)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode haskell-mode language-mode
    (:name "Haskell"
     :keymap *haskell-mode-keymap*
     :syntax-table *haskell-syntax-table*
     :mode-hook *haskell-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "--"))

(define-file-type ("hs") haskell-mode)
