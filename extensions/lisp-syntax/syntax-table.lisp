(defpackage :lem-lisp-syntax.syntax-table
  (:use :cl :lem)
  (:export :*syntax-table*)
  #+sbcl
  (:lock t))
(in-package :lem-lisp-syntax.syntax-table)

(flet ((f (c1 c2 step-fn)
         (when c1
           (when (and (member c1 '(#\#))
                      (or (alphanumericp c2)
                          (member c2 '(#\+ #\-))))
             (funcall step-fn)))))

  (defun skip-expr-prefix-forward (point)
    (f (character-at point 0)
       (character-at point 1)
       (lambda ()
         (character-offset point 2))))

  (defun skip-expr-prefix-backward (point)
    (f (character-at point -2)
       (character-at point -1)
       (lambda ()
         (character-offset point -2)))))

(defvar *syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline #\page)
   :symbol-chars '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\% #\: #\@ #\[ #\]
                   #\^ #\{ #\} #\~ #\# #\|)
   :paren-pairs '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\")
   :escape-chars '(#\\)
   :fence-chars '(#\|)
   :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
   :expr-prefix-forward-function 'skip-expr-prefix-forward
   :expr-prefix-backward-function 'skip-expr-prefix-backward
   :line-comment-string ";"
   :block-comment-pairs '(("#|" . "|#"))))
