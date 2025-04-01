(defsystem "lem-swift-mode"
  :depends-on ("lem" "yason" "lem-lsp-mode")
  :serial t
  :components ((:file "swift-mode") (:file "lsp-config")))
