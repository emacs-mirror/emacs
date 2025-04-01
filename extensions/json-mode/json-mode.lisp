(defpackage :lem-json-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*json-mode-hook*
           :json-mode))
(in-package :lem-json-mode)

(defvar *js-floating-point-literals* "\\b([+-]?[1-9]\\d*(.\\d)?([Ee][+-]?\\d+)?)\\b")
(defvar *js-integer-literals* "\\b([1-9]\\d*|0+|0[bB][01]+|0[oO][0-7]+|0[xX][\\da-fA-F]+)\\b")
(defvar *js-boolean-literals* "(true|false)")
(defvar *js-null-literals* "(null)")

(defvar *js-white-space* (list (code-char #x9) (code-char #xb) (code-char #xc)
                                (code-char #x20) (code-char #xa0)))
(defvar *js-line-terminators* (list (code-char #x0a) (code-char #x0d)
                                     (code-char #x2028) (code-char #x2029)))

(defvar *js-spaces* (append *js-white-space* *js-line-terminators*))

(defun make-tmlanguage-js ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-string-region "\"")
                    (make-tm-match *js-boolean-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-null-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-integer-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-floating-point-literals*
                                   :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *json-syntax-table*
  (let ((table (make-syntax-table
                :space-chars *js-spaces*
                :symbol-chars '(#\:)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")))
        (tmlanguage (make-tmlanguage-js)))
    (set-syntax-parser table tmlanguage)
    table))

(defun js-prettier (buf)
  "Format a JavaScript buffer with prettier."
  (let ((file (buffer-filename buf)))
    (uiop:run-program 
     (format nil "prettier -w ~a" file)
     :ignore-error-status t))
  (revert-buffer t))

(define-major-mode json-mode language-mode
    (:name "JSON"
     :keymap *json-mode-keymap*
     :syntax-table *json-syntax-table*
     :mode-hook *json-mode-hook*
     :formatter #'js-prettier)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent))

(define-file-type ("json") json-mode)
