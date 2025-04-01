(defpackage :lem-asm-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*asm-mode-hook*
           :asm-mode)
  #+sbcl
  (:lock t))
(in-package :lem-asm-mode)

#|
  I referenced softly these documents:

  - [GNU as](https://sourceware.org/binutils/docs-2.34/as/index.html)
  - [rgbasm: assembler for gameboy development](https://rednex.github.io/rgbds/rgbasm.5.html)

|#

(defun make-tmlanguage-asm ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region "\"" "\"" :name 'syntax-string-attribute)
                   (make-tm-match "[ \\t]*(;.*)"
                                  :captures (vector nil
                                                    (make-tm-name 'syntax-comment-attribute)))
                   (make-tm-match "^(\\.?[\\S]+):"
                                  :captures (vector nil
                                                    (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match "^\\t(\\.?\\S+)[ \\t]*"
                                  :captures (vector nil
                                                    (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match "^\\S+" :name 'syntax-function-name-attribute)
                   (make-tm-match "^\\S+" :name 'syntax-function-name-attribute)
                   (make-tm-match "\\$[0-9a-fA-F]+" :name 'syntax-constant-attribute)
                   (make-tm-match "#\\S+" :name 'syntax-constant-attribute)
                   (make-tm-match "=(\\S+)"
                                  :captures (vector nil
                                                    (make-tm-name 'syntax-function-name-attribute))))))
    (make-tmlanguage :patterns patterns)))

(defvar *asm-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab)
                :string-quote-chars '(#\")
                :line-comment-string ";"))
        (tmlanguage (make-tmlanguage-asm)))
    (set-syntax-parser table tmlanguage)
    table))

(defun asm-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column tab-width))))

(define-major-mode asm-mode language-mode
  (:name "Asm"
   :keymap *asm-mode-keymap*
   :syntax-table *asm-syntax-table*
   :mode-hook *asm-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) t
        (variable-value 'tab-width) 8
        (variable-value 'calc-indent-function) 'asm-calc-indent
        (variable-value 'line-comment) ";"))

(define-command asm-insert-tab () ()
  (insert-character (current-point) #\Tab))

(define-key *asm-mode-keymap* "Tab" 'asm-insert-tab)

(define-file-type ("asm" "s") asm-mode)
