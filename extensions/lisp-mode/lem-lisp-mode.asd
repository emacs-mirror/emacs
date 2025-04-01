(defsystem "lem-lisp-mode"
  :depends-on ("alexandria"
               "trivial-types"
               "usocket"
               "micros"
               "trivia"
               "uiop"
               "lem-lisp-syntax"
               "lem"
               "lem-process"
               "lem-lsp-mode")
  :serial t
  :components ((:file "test-api")
               (:file "errors")
               (:file "ui-mode")

               (:file "rpc")
               (:file "reader")
               (:file "connections")
               (:file "connection")

               (:file "message-dispatcher")
               (:file "grammar")
               (:file "implementation")
               (:file "internal-package")
               (:file "message")
               (:file "file-conversion")

               (:file "ext/detective")
               (:file "ext/completion")

               (:file "lisp-mode")
               (:file "message-definitions")
               (:file "repl")
               (:file "apropos-mode")

               (:file "ext/exporter")
               (:file "ext/inspector")
               (:file "ext/eval")
               (:file "ext/sldb")
               (:file "ext/hyperspec")
               (:file "ext/autodoc")
               (:file "ext/paren-coloring")
               (:file "ext/defstruct-to-defclass")
               (:file "ext/quickdocs")
               (:file "ext/package-inferred-system")
               (:file "ext/organize-imports")
               (:file "ext/connection-list")
               (:file "ext/self-insert-hook")
               (:file "ext/trace")
               (:file "ext/class-browser")
               (:file "ext/macroexpand")
               (:file "ext/test-runner")
               (:file "ext/utopian")
               (:file "ext/highlight")

               (:file "read-only-sources")

               (:file "package")))

(defsystem "lem-lisp-mode/v2"
  :depends-on ("lem-lisp-mode")
  :serial t
  :pathname "v2"
  :components ((:file "lsp-config")))
