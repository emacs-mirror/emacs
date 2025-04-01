(defsystem "lem-paredit-mode"
  :depends-on ("lem")
  :components ((:file "paredit-mode"))
  :in-order-to ((test-op (test-op "lem-paredit-mode/tests"))))

(defsystem "lem-paredit-mode/tests"
  :depends-on ("lem"
               "lem-lisp-mode"
               "lem-lisp-syntax"
               "lem-paredit-mode"
               "rove"
               "cl-ppcre")
  :components
  ((:module "tests"
    :components
    ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))