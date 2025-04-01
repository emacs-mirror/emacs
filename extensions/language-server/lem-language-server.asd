#+sbcl
(require :sb-concurrency)

(defsystem "lem-language-server"
  :depends-on ("alexandria"
               "jsonrpc"
               "usocket"
               "log4cl"
               "quri"
               "cl-change-case"
               "async-process"
               "micros"
               "lem"
               "lem-lisp-syntax"
               "lem-lsp-base")
  :serial t
  :components ((:file "micros-client")
               (:file "package")
               (:file "config")
               (:file "editor-utils")
               (:file "method")
               (:file "server")
               (:file "text-document")
               (:file "eval")
               (:file "commands")
               (:module "controller"
                :components ((:file "lifecycle")
                             (:file "document-synchronization")
                             (:file "language-features")
                             (:file "workspace")
                             (:file "window")
                             (:file "commands")))))

(defsystem "lem-language-server/cli"
  :depends-on ("lem-language-server"
               "command-line-arguments")
  :serial t
  :components ((:file "cli")))
