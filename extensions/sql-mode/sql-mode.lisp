(defpackage :lem-sql-mode
  (:use :cl
        :lem
        :lem/language-mode
        :lem/language-mode-tools)
  (:import-from
   :lem/tmlanguage
   :load-tmlanguage)
  (:export
   :sql-mode))
(in-package :lem-sql-mode)

(defun make-tmlanguage-sql ()
  (load-tmlanguage
   (merge-pathnames "sql.tmLanguage.json"
                    (asdf:system-source-directory :lem-sql-mode))))

(defvar *syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '()
                :line-comment-string "--")))
    (set-syntax-parser table (make-tmlanguage-sql))
    table))

(define-major-mode sql-mode language-mode
    (:name "SQL"
     :keymap *sql-mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *sql-mode-hoook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'calc-indent
        (variable-value 'line-comment) "--"
        ;; (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        ;; (variable-value 'end-of-defun-function) 'end-of-defun
        ))

(defun calc-indent (point)
  (declare (ignore point)))

(defun beginning-of-defun (point n)
  (declare (ignore point n)))

(defun end-of-defun (point n)
  (declare (ignore point n)))

(define-file-type ("sql") sql-mode)
