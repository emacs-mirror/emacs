(defpackage :lem-terraform-mode
  (:use :cl
        :lem
        :lem/language-mode
        :lem/language-mode-tools)
  (:export :terraform-mode))
(in-package :lem-terraform-mode)

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-terraform ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region '(:sequence "//") "$" :name 'syntax-comment-attribute)
                   (make-tm-region '(:sequence "#") "$" :name 'syntax-comment-attribute)
                   (make-tm-region '(:sequence "/*")
                                   '(:sequence "*/")
                                   :name 'syntax-comment-attribute)
                   (make-tm-match (tokens :word-boundary
                                          '("provider"
                                            "resource"
                                            "data"
                                            "module"
                                            "variable"
                                            "output"
                                            "locals"
                                            "terraform"
                                            "count"
                                            "depends_on"
                                            "for_each"
                                            "lifecycle"
                                            "provisioner"
                                            "connection"
                                            "sensitive"
                                            "ignore_changes"
                                            "create_before_destroy"
                                            "prevent_destroy"
                                            "dynamic"
                                            "true"
                                            "false"
                                            "null"
                                            "if"
                                            "else"
                                            "for"
                                            "in"
                                            "map"
                                            "list"
                                            "object"
                                            "var"
                                            "local"
                                            "each"
                                            "self"
                                            "path"
                                            "zipmap"
                                            "cidrsubnet"
                                            "element"
                                            "lookup"))
                                  :name 'syntax-keyword-attribute)
                   (make-tm-string-region "\"")
                   (make-tm-string-region "'")
                   (make-tm-string-region "\"\"\"")
                   (make-tm-string-region "'''"))))
    (make-tmlanguage :patterns patterns)))

(defvar *syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/")))))
    (set-syntax-parser table (make-tmlanguage-terraform))
    table))

(define-major-mode terraform-mode language-mode
    (:name "Terraform"
     :keymap *terraform-mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *terraform-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-terraform-mode/indent::calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'lem-terraform-mode/indent:beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-terraform-mode/indent:end-of-defun))

(define-file-type ("tf") terraform-mode)
