(defsystem "lem-patch-mode"
  :depends-on ("lem"
               "cl-ppcre")
  :serial t
  :components ((:file "patch-mode")))
