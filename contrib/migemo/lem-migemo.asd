;;don't edit
(defsystem "lem-migemo"
  :class :package-inferred-system
  :components((:file "main"))
  :depends-on("lem"
              :cl-migemo))
#+roswell(UNLESS (ASDF/SYSTEM:FIND-SYSTEM "cl-migemo" NIL)
  (UIOP/PACKAGE:SYMBOL-CALL "ROSWELL" "ROSWELL"
                            '("install" "snmsts/cl-migemo")))
