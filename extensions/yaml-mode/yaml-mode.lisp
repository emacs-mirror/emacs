(defpackage :lem-yaml-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*yaml-mode-hook*
           :yaml-mode))
(in-package :lem-yaml-mode)

#| link: https://yaml.org/spec/1.2/spec.html |#

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tm-line-comment (sepalator)
  (make-tm-region sepalator "$" :name 'syntax-comment-attribute))

(defun make-tmlanguage-yaml ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-line-comment "#")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match (tokens nil '("-" "?" ":" "," "[" "]" "{" "}"))
                                   :name 'syntax-builtin-attribute)
                    (make-tm-match (tokens :word-boundary '("true" "false"))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match `(:sequence
                                     :start-anchor
                                     (:greedy-repetition 0 nil :whitespace-char-class)
                                     ,(ppcre:parse-string "([a-zA-Z0-9_]|-)+"))
                                   :name 'syntax-function-name-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *yaml-syntax-table*
  (let ((table (make-syntax-table
                :symbol-chars '(#\-)
                :string-quote-chars '(#\" #\')))
        (tmlanguage (make-tmlanguage-yaml)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode yaml-mode language-mode
    (:name "Yaml"
     :keymap *yaml-mode-keymap*
     :syntax-table *yaml-syntax-table*
     :mode-hook *yaml-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'yaml-calc-indent
        (variable-value 'line-comment) "#"))

(defun yaml-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(define-file-type ("yaml" "yml") yaml-mode)
