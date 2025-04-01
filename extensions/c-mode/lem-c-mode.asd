(defsystem "lem-c-mode"
  :depends-on ("lem" "lem-lisp-mode")
  :serial t
  :components ((:file "grammar")
               (:file "format")
               (:file "c-mode")))
