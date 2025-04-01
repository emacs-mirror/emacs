(defpackage :lem/language-mode-tools
  (:use :cl :lem)
  (:export :make-tm-string-region
           :make-tm-line-comment-region
           :make-tm-block-comment-region)
  #+sbcl
  (:lock t))
(in-package :lem/language-mode-tools)

(defun make-tm-string-region (sepalator &key (name 'syntax-string-attribute)
                                             (patterns (make-tm-patterns (make-tm-match "\\\\."))))
  (make-tm-region `(:sequence ,sepalator)
                  `(:sequence ,sepalator)
                  :name name 
                  :patterns patterns))

(defun make-tm-line-comment-region (start)
  (make-tm-region start "$" :name 'syntax-comment-attribute))

(defun make-tm-block-comment-region (start end)
  (make-tm-region `(:sequence ,start)
                  `(:sequence ,end)
                  :name 'syntax-comment-attribute))
