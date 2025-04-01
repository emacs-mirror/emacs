(defsystem "lem-lisp-syntax"
  :depends-on ("lem" "cl-ppcre" "micros" "trivia")
  :serial t
  :components ((:file "indent")
               (:file "syntax-table")
               (:file "misc")
               (:file "enclosing")
               (:file "parse-for-autodoc")
               (:file "defstruct-to-defclass")
               (:file "lem-lisp-syntax")))
