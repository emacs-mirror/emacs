# Changes

## 2.4

### Breaking changes

- `use-package` no longer requires `diminish` as a dependency, allowing people
  to decide whether they want to use diminish or delight. This means that if
  you do use diminish, you'll now need to pull it into your configuration
  before any use of the `:diminish` kewyord. For example:

  ``` elisp
      (use-package diminish :ensure t)
  ```

- Emacs 24.3 or higher is now a requirement.

- The `:defer-install` keyword has been removed. It may reappear as an add-on
  module for use-package in a future release. See issue #442 for more details.

- There is no longer a `use-package-debug` option, since `use-package-verbose`
  already has the possible value of `debug`.

- The ordering of several elements of `use-package-keywords` have changed; if
  you had previously customized this (or were an extension author adding to
  this list), you may need to rework your changes.

- For extension authors, the way `:commands` are propagated down for
  autoloading has changed. They used to be passed through the `state`
  parameter, but are now done as an extension to `rest`. Please see
  `use-package-handler/:bind` for a canonical example.

- For extension authors, if you add a keyword to `use-package-keywords` whose
  presence should indicate deferred loading, please also add it to
  `use-package-deferring-keywords`.

### Other changes

- Upgrade license to GPL 3.

- If `use-package-verbose` is set to the symbol `debug`, any evaluation errors
  during package configuration will cause a complete report to be written to a
  `*use-package*` buffer, including: the text of the error, the `use-package`
  declaration that caused the error, the post-normalized form of this
  declaration, and the macro-expanded version (without verbosity-related
  code). Note that this still does not help if there are parsing errors, which
  cause Emacs to register a Lisp error at startup time.

- New customization variable `use-package-deferring-keywords`, mainly intended
  for use by extension packages, indicates keywords that, if used without
  `:demand`, cause deferred loading (as if `:defer t` had been specified).

- New `:hook` keyword.

- New keywords `:custom (foo1 bar1) (foo2 bar2)` etc., and `:custom-face`.

- New `:magic` and `:magic-fallback` keywords.

- New `:defer-install` keyword.

- New customization variable `use-package-enable-imenu-support`.

- Allow `:diminish` to take no arguments.

- Support multiple symbols passed to `:after`, and a mini-DSL using `:all` and
  `:any`.

- `:mode` and `:interpreter` can now accept `(rx ...)` forms.

- Using `:load-path` without also using `:ensure` now implies `:ensure nil`.

- `:bind (:map foo-map ...)` now defers binding in the map until the package
  has been loaded.

- Print key bindings for keymaps in `describe-personal-keybindings`.

- When `use-package-inject-hooks` is non-nil, always fire `:init` and
  `:config` hooks.

- Documentation added for the `:after`, `:defer-install`, `:delight`,
  `:requires`, `:when` and `:unless` keywords.

- New undocumented (and currently experimental) keyword `:load` may be used to
  change the name of the actual package loaded, rather than the package name,
  and may even add other names. For example: `(use-package auctex :load
  tex-site)`. This keyword is used internally to generate the `require` for a
  package, so that deferral is simply a matter of not generating this keyword.

- The source code is now broken into several files, so that certain optional
  features (diminish, delight, ensure) may be maintained separately from the
  core functionality.

### Bug fixes

- Repeating a bind no longer causes duplicates in personal-keybindings.
- When byte-compiling, correctly output declare-function directives.
- Append to *use-package* when debugging, don't clear it.
- Don't allow :commands, :bind, etc., to be given an empty list.
- Explicit :defer t should override use-package-always-demand.
