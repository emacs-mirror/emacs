(defpackage :lem-erlang-mode/lsp-config
  (:use :cl :lem-lsp-mode :lem-lsp-base/type))

(in-package :lem-erlang-mode/lsp-config)

(defvar *lsp-erlang-elp-server-path*
  (uiop:native-namestring "/usr/local/bin/elp")
   "Adapt to your system's ELP path.")

(defvar *lsp-erlang-elp-log-path*
  (uiop:native-namestring "~/tmp/elp.log"))

(defvar *lsp-erlang-server-command*
  `(,*lsp-erlang-elp-server-path*
    "server"
    "--log-file" ,*lsp-erlang-elp-log-path*
    "--no-log-buffering")
    )

(define-language-spec (erlang-spec lem-erlang-mode:erlang-mode)
  :language-id "erlang"
  :root-uri-patterns '("rebar3")
  :command *lsp-erlang-server-command*
  :install-command ""
  :readme-url "https://github.com/WhatsApp/erlang-language-platform"
  :connection-mode :stdio)
