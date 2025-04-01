#| link : http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html |#
(defpackage :lem-posix-shell-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*posix-shell-mode-hook*
           :posix-shell-mode))
(in-package :lem-posix-shell-mode)

(defparameter *reserved-words*
  '("case" "do" "done" "elif" "else"
    "esac" "fi" "for" "if" "in"
    "then" "until" "while"
    "function" "select"))

(defparameter *shell-variables*
  '("ENV" "HOME" "IFS" "LANG" "LC_ALL"
    "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES" "LINENO" "NLSPATH"
    "PATH" "PPID" "PS1" "PS2" "PS4" "PWD"))

(defparameter *special-built-in-utilities*
  '("break" "colon" "continue" "dot" "eval"
    "exec" "exit" "export" "readonly" "return"
    "set" "shift" "times" "trap" "unset"))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun parameter-expression ()
  (let ((special-parameters "[*@#?\\-$!0]")
        (name "([a-zA-Z_][0-9a-zA-Z_]*)"))
    (format nil "\\$((~a)|(~a)|(\\{~a\\}))" special-parameters name name)))

(defun make-tmlanguage-posix-shell ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
                    (make-tm-match "\\\\'")
                    (make-tm-match "\\\\\"")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match (tokens :word-boundary
                                           *reserved-words*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (parameter-expression)
                                   :name 'syntax-variable-attribute)
                    (make-tm-match (tokens :word-boundary
                                           *shell-variables*)
                                   :name 'syntax-variable-attribute)
                    (make-tm-match (tokens :word-boundary
                                           *special-built-in-utilities*)
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *posix-shell-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\[ . #\])
                               (#\{ . #\}))
                :string-quote-chars '(#\" #\')
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-posix-shell)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode posix-shell-mode language-mode
    (:name "posix-shell"
     :keymap *posix-shell-mode-keymap*
     :syntax-table *posix-shell-syntax-table*
     :mode-hook *posix-shell-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'calc-indent-function) 'posix-shell-calc-indent
        (variable-value 'line-comment) "#"))

(defun posix-shell-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(define-file-type ("sh" "bashrc" "profile") posix-shell-mode)
(define-program-name-with-mode ("sh" "bash") posix-shell-mode)
