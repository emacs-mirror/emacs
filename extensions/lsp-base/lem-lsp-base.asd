(defsystem "lem-lsp-base"
  :depends-on ("lem" "cl-change-case" "bordeaux-threads" "jsonrpc" "trivia.level2" "quri")
  :serial t
  :components ((:file "yason-utils")
               (:file "type")
               (:file "protocol-generator")
               (:file "protocol-3-17")
               (:file "converter")
               (:file "utils")))
