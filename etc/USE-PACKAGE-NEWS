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

- New `:hook` keyword.
- Documentation added for the `:after`, `:defer-install`, `:delight`,
  `:requires`, `:when` and `:unless` keywords.
- When use-package-inject-hooks is non-nil, always fire init/config hooks.
- Print key bindings for keymaps in `describe-personal-keybindings`.
- Allow `:diminish` to take no arguments.
- Add `:custom (foo1 bar1) (foo2 bar2)` etc., and `:custom-face`.
- Add `:magic` and `:magic-fallback` keywords.
- Add `:defer-install` keyword.
- New customization variable `use-package-enable-imenu-support`.
- Upgrade license to GPL 3.
- `:bind (:map foo-map ...)` now defers binding in the map until the package
  has been loaded.
- Support multiple symbols passed to `:after`, and a mini-DSL using `:all` and
  `:any`.
- `:mode` and `:interpreter` can now accept `(rx ...)` forms.

### Bug fixes

- Repeating a bind no longer causes duplicates in personal-keybindings.
- When byte-compiling, correctly output declare-function directives.
- Append to *use-package* when debugging, don't clear it.
- Don't allow :commands, :bind, etc., to be given an empty list.
- Explicit :defer t should override use-package-always-demand.

