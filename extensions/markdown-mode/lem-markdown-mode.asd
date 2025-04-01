(defsystem "lem-markdown-mode"
  :depends-on ("lem"
               "3bmd"
               "3bmd-ext-code-blocks"
               "lisp-preprocessor"
               "trivial-ws"
               "trivial-open-browser"
               "lem-lisp-mode"
               "lem-elisp-mode"
               "lem-posix-shell-mode"
               "lem-json-mode")
  :serial t
  :components ((:file "internal")
               (:file "languages")
               (:file "syntax-parser")
               (:file "markdown-mode")
               (:module "preview"
                :serial t
                :components ((:file "preview")
                             (:file "external-browser")
                             (:file "html-buffer")))))
