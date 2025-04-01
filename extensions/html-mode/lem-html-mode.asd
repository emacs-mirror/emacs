(defsystem "lem-html-mode"
  :depends-on ("lem"
               "lem-xml-mode"
               "cl-ppcre")
  :serial t
  :components ((:file "html-mode")))
