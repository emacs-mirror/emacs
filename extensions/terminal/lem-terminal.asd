(defsystem "lem-terminal"
  :depends-on ("lem")
  :serial t
  :components ((:file "ffi")
               (:file "terminal")
               (:file "terminal-mode")))
