(defsystem "lem-elisp-mode"
  :depends-on ("lem"
               "jsonrpc"
               "lem-lisp-mode"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process")
  :serial t
  :components ((:file "rpc")
               (:file "elisp-mode")
               (:file "run-elisp")))