(defpackage :lem-typescript-mode/lsp-config
  (:use :cl))
(in-package :lem-typescript-mode/lsp-config)

(lem-lsp-mode:define-language-spec (typescript-spec lem-typescript-mode:typescript-mode)
  :language-id "typescript"
  :root-uri-patterns '("package.json" "tsconfig.json")
  :command '("typescript-language-server" "--stdio")
  :install-command "npm install -g typescript-language-server typescript"
  :readme-url "https://github.com/typescript-language-server/typescript-language-server"
  :connection-mode :stdio)
