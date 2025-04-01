(defsystem "lem-vi-mode"
  :depends-on ("esrap"
               "closer-mop"
               "lem"
               "cl-ppcre"
               "parse-number"
               "cl-package-locks"
               "alexandria"
               "split-sequence"
               "lem-lisp-mode"
               "trivial-types")
  :components ((:file "core")
               (:file "leader" :depends-on ("core"))
               (:file "options" :depends-on ("utils"))
               (:file "word" :depends-on ("options"))
               (:file "modeline" :depends-on ("core"))
               (:file "states" :depends-on ("core" "modeline"))
               (:file "visual" :depends-on ("core" "states"))
               (:file "text-objects" :depends-on ("core" "visual" "word"))
               (:file "registers" :depends-on ("core"))
               (:file "jumplist")
               (:module "commands-utils"
                :pathname "commands"
                :depends-on ("core" "jumplist" "visual" "states")
                :components ((:file "utils")))
               (:file "commands" :depends-on ("core" "commands-utils" "word" "visual" "jumplist" "states" "registers" "window"))
               (:file "ex-core" :depends-on ("visual"))
               (:file "ex-parser" :depends-on ("ex-core"))
               (:file "ex-command" :depends-on ("ex-core" "options" "utils"))
               (:file "ex" :depends-on ("core" "ex-parser" "visual" "registers"))
               (:file "binds" :depends-on ("states" "commands" "ex" "visual"))
               (:file "special-binds" :depends-on ("core"))
               (:file "window" :depends-on ("options"))
               (:file "vi-mode" :depends-on ("core" "options" "ex" "commands" "states" "window"))
               (:file "utils"))
  :in-order-to ((test-op (test-op "lem-vi-mode/tests"))))

(defsystem "lem-vi-mode/tests"
  :depends-on ("lem"
               "lem-vi-mode"
               "lem-fake-interface"
               "rove"
               "cl-ppcre"
               "cl-interpol"
               "named-readtables"
               "alexandria")
  :components
  ((:module "tests"
    :depends-on ("utils")
    :components
    ((:file "motion")
     (:file "operator")
     (:file "visual")
     (:file "commands")
     (:file "ex")
     (:file "text-objects")
     (:file "registers")
     (:file "kbdmacro")
     (:file "jumplist")
     (:file "options")))
   (:file "utils"
    :pathname "tests/utils"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
