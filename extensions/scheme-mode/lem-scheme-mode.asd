(defsystem "lem-scheme-mode"
  :depends-on ("alexandria"
               "trivial-types"
               "usocket"
               "trivia"
               "uiop"
               "swank"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process"
               "lem")
  :serial t
  :components ((:file "syntax-data")
               (:file "syntax-indent")
               (:file "syntax-syntax-table")
               (:file "syntax-misc")
               (:file "syntax-parse")
               (:file "lem-scheme-syntax")
               (:file "errors")
               (:file "swank-protocol")
               (:file "package")
               (:file "grammar")
               (:file "scheme-mode")
               (:file "swank-connection")
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "eval")
               (:file "repl")))
