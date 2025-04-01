(defpackage :lem-go-mode/lsp-config
  (:use :cl
         :lem-lsp-mode
         :lem-lsp-base/type))
(in-package :lem-go-mode/lsp-config)

(define-language-spec (go-spec lem-go-mode:go-mode)
  :language-id "go"
  :root-uri-patterns '("go.mod")
  :command (lambda (port) `("gopls" "serve" "-port" ,(princ-to-string port)))
  :install-command "go install golang.org/x/tools/gopls@latest"
  :readme-url "https://github.com/golang/tools/tree/master/gopls"
  :connection-mode :tcp)

(defmethod spec-initialization-options ((spec go-spec))
  (make-lsp-map "completeUnimported" +true+
                "matcher" "fuzzy"))
