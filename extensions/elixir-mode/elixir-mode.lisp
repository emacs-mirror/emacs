(defpackage :lem-elixir-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*elixir-mode-hook*
           :elixir-mode
           :*elixir-syntax-table*
           :*elixir-mode-keymap*))

(in-package :lem-elixir-mode)

(defvar *elixir-keywords*
  '("case" "cond" "for" "if" "quote" "raise" "receive" "send" 
    "super" "throw" "try" "unless" "unquote" "unquote_splicing" "with" 
    
    "import" "require" "use" "alias"
    
    "def" "defp" "defmodule" "defprotocol"
    "defmacro" "defmacrop" "defdelegate"
    "defexception" "defstruct" "defimpl"
    "defguard" "defguardp" "defcallback"
    "defoverridable" 
    "end" "after" "else" "rescue" "catch" 

    "not" "and" "or" "when" "in"

    "fn" "do" "end" "after" "else" "rescue" "catch"

    "->" "<-" "=>" "|>"))

(defvar *elixir-boolean-literals*
  '("true" "false"))

(defvar *elixir-null-literal*
  '("nil"))

;; From:  http://elixir-lang.org/getting-started/basic-operators.html
(defvar *elixir-operators*
  '("+" "*" "++" "--" "<>" "||" "&&" "!"
    "==" "!=" "===" "!==" ">=" "<="))


(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))


(defun make-tmlanguage-elixir ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match ":\\w+" 
                                   :name 'syntax-constant-attribute) 
                    (make-tm-match "\\w+:"
                                   :name 'syntax-constant-attribute)
                    (make-tm-line-comment-region "#")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-string-region "\"\"\"")
                    (make-tm-match (tokens :word-boundary 
                                           (append *elixir-boolean-literals*
                                                   *elixir-null-literal*))
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens :word-boundary *elixir-keywords*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (tokens nil *elixir-operators*)
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *elixir-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\"")
                                      ("'''" . "'''"))
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-elixir)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode elixir-mode language-mode
    (:name "Elixir"
     :keymap *elixir-mode-keymap*
     :syntax-table *elixir-syntax-table*
     :mode-hook *elixir-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'line-comment) "#"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(defun beginning-of-defun (point n)
  (loop :with regex = "^ *def(?:callback|delegate|exception|guardp?|impl|m(?:acrop?|odule)|overridable|p(?:rotocol)?|struct)?"
        :repeat n 
        :do (search-backward-regexp point regex)))


(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (unless (search-forward-regexp p "^  end") (return)))
    (line-start p)
    (move-point point p)))

(define-file-type ("ex" "exs" "elixir") elixir-mode)
