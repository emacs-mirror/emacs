#|
link : https://asciidoctor.org/docs/asciidoc-syntax-quick-reference/
|#

(defpackage :lem-asciidoc-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*asciidoc-mode-hook*
           :asciidoc-mode))
(in-package :lem-asciidoc-mode)

(defun make-tmlanguage-asciidoc ()
  (labels ((tm-builtin (pattern)
             (make-tm-match pattern :name 'syntax-builtin-attribute))
           (tm-constant (pattern)
             (make-tm-match pattern :name 'syntax-constant-attribute))
           (tm-function-name (pattern)
             (make-tm-match pattern :name 'syntax-function-name-attribute))
           (tm-string (pattern)
             (make-tm-match pattern :name 'syntax-string-attribute)))
    (let* ((patterns (make-tm-patterns
                      ;; block
                      (make-tm-block-comment-region "----" "----")
                      ;; line breaks
                      (tm-constant "(\\+)|(\\[%hardbreaks\\])")
                      ;; lead style
                      (tm-constant "\\[\\.lead\\]")
                      ;; admonition
                      (tm-constant "((NOTE)|(TIP)|(IMPORTANT)|(WARNING)|(CAUTION))\\:")
                      ;; bold and italic
                      (tm-string "(\\*[^*]+\\*)|(\\*\\*[^*]+\\*\\*)")
                      (tm-string "(_[^_]+_)|(__[^_]+__)")
                      (tm-string "(`[^`]+`)|(``[^`]+``)")
                      ;; curved quotation marks
                      (tm-string "\"`[^`\"]+`\"")
                      (tm-string "'`[^`']+`'")
                      ;; section
                      (tm-function-name "={1,6} .+")
                      ;; unordered list
                      (tm-builtin "^(\\*){1,5} ")
                      (tm-builtin "^-{1,5} ")
                      ;; check list
                      (tm-builtin "^((\\*)|-) \\[[ x*]\\] ")
                      ;; ordered list
                      (tm-builtin "^(\\.){1,5} ")
                      (tm-builtin "^\\d\\. ")
                      ;; subscript and superscript
                      (tm-string "~[^~]+~")
                      (tm-string "\\^[^^]+\\^")
                      ;; tables
                      (tm-builtin "^\\|(\\=)+$")
                      (tm-builtin "^\\[.+\\]"))))
      (make-tmlanguage :patterns patterns))))

(defvar *asciidoc-syntax-table*
  (let ((table (make-syntax-table :space-chars '(#\space #\tab #\newline)))
        (tmlanguage (make-tmlanguage-asciidoc)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode asciidoc-mode language-mode
    (:name "AsciiDoc"
     :keymap *asciidoc-mode-keymap*
     :syntax-table *asciidoc-syntax-table*
     :mode-hook *asciidoc-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-file-type ("adoc") asciidoc-mode)
