# vi-mode

## Usage

To enable, add the following code to `~/.lem/init.lisp`:

```common-lisp
(lem-vi-mode:vi-mode)
```

## Defining keymaps

```common-lisp
;; NORMAL mode
(define-key lem-vi-mode:*normal-keymap* "q" 'quit-window)
(define-key lem-vi-mode:*normal-keymap* "Space @" 'paredit-splice)

;; INSERT mode
(define-key lem-vi-mode:*insert-keymap* "(" 'paredit-insert-paren)
(define-key lem-vi-mode:*insert-keymap* ")" 'paredit-close-parenthesis)
```

## User Settings

### vi-operator-surrounding-blanks

This is an editor variable that allows capturing the surround blanks (Space/Tab) in operator pending mode (eg da", ya", va"))
This currently only works for double quotes. This feature is not in vanilla vi/vim.

```common-lisp To enable this variable:
(setf (variable-value 'lem-vi-mode/text-objects:vi-operator-surrounding-blanks :global) t)
```

#### Example

```
"cursor inside these quotes"    "but not inside these"
```
Here va" will select inside < >:
```
<"cursor inside these quotes"    >"but not inside these"
```

## Options

Vi-mode options are global settings, similarly to Vim.

They can be set with `:set` command, or a function `option-value` in `~/.lem/init.lisp`, like:

```common-lisp
(setf (lem-vi-mode:option-value "autochdir") t)
```

Here's a list of all options currently implemented:

* `autochdir`: Boolean to change the current directory to the buffer's directory automatically.
  * Default: `nil` (don't change the current directory)
  * Aliases: `acd`
* `number`: Boolean to show the line number.
  * Default: `nil` (don't show)
  * Aliases: `nu`

## Unit Tests
    
### Command Line
    
```bash
qlot install
.qlot/bin/rove extensions/vi-mode/lem-vi-mode.asd
```
    
### In Lem
    
Use `C-c C-r` or:
```common-lisp Example Test
  (rove:run-test 'lem-vi-mode/tests/text-objects::word-object)
```
