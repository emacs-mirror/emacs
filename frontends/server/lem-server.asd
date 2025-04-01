(defsystem "lem-server"
  :depends-on ("lem"
               "lem/extensions"
               "jsonrpc"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/websocket"
               "jsonrpc/transport/local-domain-socket"
               "command-line-arguments")
  :serial t
  :components ((:file "jsonrpc-stdio-patch")
               (:file "config")
               (:file "utils")
               (:file "view")
               (:file "mouse")
               (:file "main")))
