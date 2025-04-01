(defsystem "lem-go-mode"
  :depends-on ("lem" "yason" "lem-lsp-mode")
  :serial t
  :components ((:file "go-mode")
               (:file "lsp-config")))
