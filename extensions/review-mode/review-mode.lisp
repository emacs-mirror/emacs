#|
link : https://reviewml.org
link : https://github.com/kmuto/review/blob/master/doc/format.ja.md
|#

(defpackage :lem-review-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*review-mode-hook*
           :review-mode))
(in-package :lem-review-mode)

(defun make-tmlanguage-review ()
  (let* ((patterns (make-tm-patterns
                    ;; headings
                    (make-tm-match "^=+(\\[/?(column)|(nonum)|(nodisp)|(notoc)\\])?.*$"
                                   :name 'syntax-constant-attribute)
                    ;; itemize
                    (make-tm-match "^ +\\*+ "
                                   :name 'syntax-keyword-attribute)
                    ;; ordered itemize
                    (make-tm-match "^ +[0-9]+\\. "
                                   :name 'syntax-keyword-attribute)
                    ;; definition list
                    (make-tm-match "^ : .*$"
                                   :name 'syntax-keyword-attribute)
                    ;; block command
                    (make-tm-match "^//.+(\\[.+\\])*{"
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match "^//}"
                                   :name 'syntax-keyword-attribute)
                    ;; inline command
                    (make-tm-match "@<[^{}]+>{[^}]+}"
                                   :name 'syntax-string-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *review-syntax-table*
  (let ((table (make-syntax-table :space-chars '(#\space #\tab #\newline)))
        (tmlanguage (make-tmlanguage-review)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode review-mode language-mode
    (:name "Re:VIEW"
     :keymap *review-mode-keymap*
     :syntax-table *review-syntax-table*
     :mode-hook *review-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-file-type ("re") review-mode)
