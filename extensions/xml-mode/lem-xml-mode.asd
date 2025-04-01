(defsystem "lem-xml-mode"
  :depends-on ("lem"
               "cl-ppcre")
  :serial t
  :components ((:file "xml-mode")))
