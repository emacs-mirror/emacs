(defpackage :lem-lua-mode/lsp-config
  (:use :cl :lem-lsp-mode :lem-lsp-base/type))
(in-package :lem-lua-mode/lsp-config)

(define-language-spec (lua-spec lem-lua-mode:lua-mode)
  :language-id "lua"
  ;; I have no idea what a correct file may identify a root of a Lua project
  :root-uri-patterns '(".git")
  :command '("lua-language-server")
  :readme-url "https://github.com/luals/lua-language-server"
  :connection-mode :stdio)
