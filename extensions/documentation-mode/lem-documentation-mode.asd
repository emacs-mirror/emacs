(defsystem "lem-documentation-mode"
  :depends-on ("lem" "lem-lisp-syntax")
  :serial t
  :components ((:file "utils")
	       (:file "internal")
	       (:file "documentation-mode")))
