(defpackage :lem-terraform-mode/lsp-config
  (:use :cl
        :lem-lsp-mode
        :lem-lsp-base/type))
(in-package :lem-terraform-mode/lsp-config)

(define-language-spec (terraform-spec lem-terraform-mode:terraform-mode)
  :language-id "terraform"
  :root-uri-patterns '()
  :command (lambda (port) `("terraform-ls" "serve" "-port" ,(princ-to-string port)))
  :install-command ""
  :readme-url "https://github.com/hashicorp/terraform-ls"
  :connection-mode :tcp)
