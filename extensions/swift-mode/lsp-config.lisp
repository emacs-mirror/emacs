(defpackage :lem-swift-mode/lsp-config
  (:use :cl :lem-lsp-mode :lem-lsp-base/type))

(in-package :lem-swift-mode/lsp-config)

(define-language-spec (swift-spec lem-swift-mode:swift-mode)
  :language-id "swift"
  :root-uri-patterns '("Package.swift")
  :command '("xcrun" "--toolchain" "swift" "sourcekit-lsp") ;; either behind $PATH or 'xcrun --toolchain swift sourcekit-lsp'
  :install-command "" ;; It kinda..has to be installed?
  :readme-url "https://github.com/apple/sourcekit-lsp"
  :connection-mode :stdio)