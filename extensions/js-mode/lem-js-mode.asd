(defsystem "lem-js-mode"
  :depends-on ("lem"
               "lem-lsp-mode"
               "lem-xml-mode")
  :serial t
  :components ((:file "js-mode")
               (:file "eslint")
               (:file "lsp-config")))

