(asdf:defsystem "lem-base16-themes-generate"
  :author "Łukasz Pankowski"
  :license "MIT"
  :depends-on ("cl-base16" "cl-mustache")
  :components ((:module "src"
                :components
                ((:file "generate")))))
