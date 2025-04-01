(defsystem "lem-shell-mode"
  :depends-on ("lem" "lem-process" "lem-lisp-mode")
  :serial t
  :components ((:file "shell-mode")))
