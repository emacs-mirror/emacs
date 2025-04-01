(defpackage :lem-elixir-mode/lsp-config
  (:use :cl :lem-lsp-mode :lem-lsp-base/type))

(in-package :lem-elixir-mode/lsp-config)

(define-language-spec (elixir-spec lem-elixir-mode:elixir-mode)
  :language-id "elixir"
  :root-uri-patterns '("mix.exs")
  :command '("sh" "language_server.sh")
  :install-command ""
  :readme-url "https://github.com/elixir-lsp/elixir-ls"
  :connection-mode :stdio)
