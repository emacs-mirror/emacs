(asdf:defsystem "lem-ollama"
  :author "garlic0x1"
  :description "An ollama client for Lem."
  :license "MIT"
  :class :package-inferred-system
  :depends-on (:lem :alexandria :bordeaux-threads :dexador :cl-json :chunga)
  :components ((:file "ollama")
               (:file "listener")))
