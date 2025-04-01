(defsystem "lem-typescript-mode"
  :depends-on ("lem" "lem-lsp-mode" "lem-js-mode")
  :serial t
  :components ((:file "typescript-mode")
               (:file "lsp-config")))
