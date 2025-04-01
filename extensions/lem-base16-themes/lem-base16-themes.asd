(asdf:defsystem "lem-base16-themes"
  :author "Łukasz Pankowski"
  :license "MIT"
  :depends-on (#:lem)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "macros")
                 (:file "themes" :depends-on ("macros"))))))
