(defpackage :lem-makefile-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:import-from #:ppcre
                #:parse-string)
  (:export :*makefile-mode-hook*
           :makefile-mode))
(in-package :lem-makefile-mode)

(defun tokens (strings &optional boundary)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-patterns-makefile ()
  (make-tm-patterns
   (make-tm-line-comment-region "#")
   (make-tm-string-region "\"")
   (make-tm-string-region "'")
   (make-tm-match "^ +.+$" :name 'syntax-warning-attribute)
   (make-tm-match `(:sequence ,(parse-string "^([a-zA-Z0-9_\\-./$() ]+)") ":")
                  :captures (vector nil (make-tm-name 'syntax-function-name-attribute) nil))
   (make-tm-match `(:sequence ,(parse-string "^([a-zA-Z_\\-. ]+)") "=")
                  :captures (vector nil (make-tm-name 'syntax-variable-attribute) nil))
   (make-tm-match "\\$\\([^)]+\\)" :name 'syntax-keyword-attribute)))

(defvar *makefile-syntax-table*
  (let ((table (make-syntax-table :space-chars '(#\space #\tab #\newline)
                                  :symbol-chars '(#\_)
                                  :paren-pairs '((#\( . #\)) (#\{ . #\}) (#\[ . #\]))
                                  :string-quote-chars '(#\' #\")))
        (parser (make-tmlanguage :patterns (make-patterns-makefile))))
    (set-syntax-parser table parser)
    table))

(define-major-mode makefile-mode language-mode
    (:name "Makefile"
     :keymap *makefile-mode-keymap*
     :syntax-table *makefile-syntax-table*
     :mode-hook *makefile-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) t
        (variable-value 'tab-width) 8
        (variable-value 'calc-indent-function) 'makefile-calc-indent
        (variable-value 'line-comment) "#"))

(defun makefile-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point)))
      (+ (point-column point) (- tab-width (rem (point-column point) tab-width))))))

(define-file-associations makefile-mode
  ((:file-namestring "Makefile")
   (:file-namestring "makefile")))
