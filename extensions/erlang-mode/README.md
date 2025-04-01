# Erlang mode for Lem

## Features:

### Syntax Highlighting

### Shell (Run Erlang)

`M-X run-erlang`

### Language Server

Erlang mode will connect to your local ELP [Erlang Language Platform](https://github.com/WhatsApp/erlang-language-platform) installation.
Configure the ELP binary's location by setting *lsp-elang-elp-server-path*:

```
(defvar *lsp-erlang-elp-server-path*
  (uiop:native-namestring "/usr/local/bin/elp")
   "Adapt to your system's ELP path.")
```

Currently, Erlang mode does not add any LSP features on its own. The integration is experimental with the purpose of figuring out how to use the existing Lem/LSP features and how to add Erlang specific features.