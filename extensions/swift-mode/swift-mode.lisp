(defpackage :lem-swift-mode
  (:use :cl :lem :lem/completion-mode :lem/language-mode)
  (:import-from :lem/tmlanguage :load-tmlanguage)
  (:export :*swift-mode-hook* :swift-mode))

(in-package :lem-swift-mode)

;; Syntax highlighting (like the go implementation) is effectively
;; a json file from Atom's system. Eventually, I will have to
;; update the json for Swift 5.9's macros and new syntax
(defvar *swift-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\#)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (load-tmlanguage
                     (merge-pathnames "swift.json"
                                      (asdf:system-source-directory :lem-swift-mode)))))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode swift-mode language-mode
    (:name "Swift"
     :keymap *swfit-mode-keymap*
     :syntax-table *swift-syntax-table*
     :mode-hook *swift-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'tab-width) 2) ;; Can be 4, Swift OSS is 2
  (setf (variable-value 'line-comment) "//")
  (setf (variable-value 'insertion-line-comment) "// "))

(define-file-type ("swift") swift-mode)
