(defsystem "lem-terraform-mode"
  :depends-on ("lem")
  :serial t
  :components ((:file "indent")
               (:file "terraform-mode")
               (:file "lsp-config")))
