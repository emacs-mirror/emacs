(defpackage :lem-patch-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*patch-mode-hook*
           :patch-mode))
(in-package :lem-patch-mode)

(define-attribute add-line-attribute
  (t :foreground :base0C))

(define-attribute delete-line-attribute
  (t :foreground :base08))

(define-attribute hunk-line-attribute
  (t :foreground :base06 :background :base01 :bold t))

(defun make-tmlanguage-patch ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match "^\\+.*$" :name 'add-line-attribute)
                    (make-tm-match "^-.*$" :name 'delete-line-attribute)
                    (make-tm-match "^\\@\\@ .*$" :name 'hunk-line-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *patch-syntax-table*
  (let ((table (make-syntax-table)))
    (set-syntax-parser table (make-tmlanguage-patch))
    table))

(define-major-mode patch-mode language-mode
    (:name "patch"
     :syntax-table *patch-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-file-type ("patch") patch-mode)
