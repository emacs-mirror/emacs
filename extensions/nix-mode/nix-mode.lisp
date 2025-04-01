(defpackage :lem-nix-mode
  (:use :cl
        :lem
        :lem/language-mode
        :lem/language-mode-tools)
  (:local-nicknames (:indent :lem-nix-mode/indent))
  (:export :nix-mode))
(in-package :lem-nix-mode)

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-nix ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region '(:sequence "#") "$" :name 'syntax-comment-attribute)
                   (make-tm-region '(:sequence "''")
                                   '(:sequence "''")
                                   :name 'syntax-string-attribute)
                   (make-tm-match (tokens :word-boundary
                                          '("true"
                                            "false"
                                            "null"))
                                  :name 'syntax-keyword-attribute)
                   (make-tm-string-region "''")
                   (make-tm-string-region "\""))))
    (make-tmlanguage :patterns patterns)))

(defvar *syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")
                :block-string-pairs '(("''" . "''"))
                :line-comment-string "#")))
    (set-syntax-parser table (make-tmlanguage-nix))
    table))

(define-major-mode nix-mode language-mode
    (:name "Nix"
     :keymap *nix-mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *nix-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 8
        (variable-value 'calc-indent-function) 'indent:calc-indent
        (variable-value 'line-comment) "#"
        (variable-value 'beginning-of-defun-function) 'indent:beginning-of-defun
        (variable-value 'end-of-defun-function) 'indent:end-of-defun
        ))

(define-file-type ("nix") nix-mode)
