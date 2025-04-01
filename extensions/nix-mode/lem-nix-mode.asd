(defsystem "lem-nix-mode"
  :depends-on ("lem")
  :serial t
  :components ((:file "indent")
               (:file "nix-mode")))
