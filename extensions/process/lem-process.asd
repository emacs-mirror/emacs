(defsystem "lem-process"
  :depends-on ("async-process" "lem")
  :serial t
  :components ((:file "package")
               (:file "process")
               (:file "stream")))
