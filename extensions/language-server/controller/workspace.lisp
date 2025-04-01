(in-package :lem-language-server)

(define-request (execute-command-request "workspace/executeCommand")
    (params lsp:execute-command-params)
  (let ((command (lsp:execute-command-params-command params))
        (arguments (lsp:execute-command-params-arguments params)))
    (execute-command command arguments)))
