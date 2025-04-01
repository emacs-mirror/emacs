(defsystem "lem-lsp-mode"
  :depends-on ("alexandria"
               "cl-package-locks"
               "jsonrpc"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/tcp"
               "quri"
               "trivia"
               "lem-process"
               "lem-language-client"
               "lem-language-server")
  :serial t
  :components ((:file "async-process-stream")
               (:file "lem-stdio-transport")
               (:file "client")
               (:file "spec")
               (:file "lsp-mode")))
