
(defsystem "lem-elixir-mode"
  :depends-on ("lem"
               "lem-lsp-mode"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process")
  :serial t
  :components ((:file "elixir-mode")
	       (:file "lsp-config")
	       #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "run-elixir")))
