(defsystem "lem-language-client"
  :depends-on ("jsonrpc"
               "lem-lsp-base"
               "cl-package-locks")
  :serial t
  :components ((:file "client")
               (:file "request")))
