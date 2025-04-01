(defsystem "lem-dot-mode"
  :depends-on ("lem"
               "cl-ppcre")
  :serial t
  :components ((:file "dot-mode")))
