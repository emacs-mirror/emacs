
(defsystem "lem-erlang-mode"
  :depends-on ("lem"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process"
               "lem-lisp-mode")
  :serial t
  :components ((:file "erlang-mode")
	       (:file "lsp-config")
	       #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "run-erlang")))
