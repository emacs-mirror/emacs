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

### Other changes

- Upgrade license to GPL 3.
- New `:hook` keyword.
- New keywords `:custom (foo1 bar1) (foo2 bar2)` etc., and `:custom-face`.
- New `:magic` and `:magic-fallback` keywords.
- New `:defer-install` keyword.
- New customization variable `use-package-enable-imenu-support`.
- Allow `:diminish` to take no arguments.
- Support multiple symbols passed to `:after`, and a mini-DSL using `:all` and
  `:any`.
- `:mode` and `:interpreter` can now accept `(rx ...)` forms.
- `:bind (:map foo-map ...)` now defers binding in the map until the package
  has been loaded.
- Print key bindings for keymaps in `describe-personal-keybindings`.
- When `use-package-inject-hooks` is non-nil, always fire `:init` and
  `:config` hooks.
- Documentation added for the `:after`, `:defer-install`, `:delight`,
  `:requires`, `:when` and `:unless` keywords.

### Bug fixes

- Repeating a bind no longer causes duplicates in personal-keybindings.
- When byte-compiling, correctly output declare-function directives.
- Append to *use-package* when debugging, don't clear it.
- Don't allow :commands, :bind, etc., to be given an empty list.
- Explicit :defer t should override use-package-always-demand.

