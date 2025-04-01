# ChangeLog

## Not released

### Fixed

* Now if color attribute does not specify foreground or background color,
  the default color is used explicitly.
  
  This fixes colour glitches in case if colour theme sets the background
  and foreground colours and Lem's frontend is NCurses.

## [v1.8]

(processed upto a4bea761)

### Added
* asm-mode
* frame structure
* frame-multiplexer
* lisp-defstruct-to-defclass command
* Added CI with github actions
* Added unit tests

### Fixed
* Fixed timer behavior in ccl (https://github.com/cxxxr/lem/issues/431)
* Fixed backward movement of s-expression in lisp
* Fixed compiler warning on lem-base and lem-core

### Changed
* Migration from optima to trivia

## [v1.7]

(processed upto bf9a92b3)

### Added

* An open collective campaign to support Lem financially!
* Prompt for directory creation when it does not exist.

  When openning a file in an non-existing directory `find-file`
  and `read-file` ask the user if the directory should be
  created before opening the file, aborting when the user
  responds 'n'.
* If the user types a wrong buffer name, `select-buffer` now asks
  whether the user wants to create it.
* Added a function `lem:indent-buffer`.
* Now it is possible to run a prebuilt Lem in a Docker container, like this:

  ```
  docker run --rm -ti -v `pwd`:/app 40ants/lem:latest
  ```

* Added ability to output infmation into the log.

  Two new options were added to control if log should be written to the file:

  * `--log-filename /tmp/lem.log` - this will output `INFO`, `WARNING`
    and `ERROR` messages to the file.
  * `--debug` - with this flag Lem will output also `DEBUG` messages to the log.
    This flag should be used along with `--log-filename`.
* Added `directory-does-not-exist` condition.
* Two new commands `move-to-beginning-of-logical-line` and `move-to-end-of-logical-line`.
  They work correctly for long lines wrapped through the screen width.
* Functions `move-to-next-virtual-line-n` and `move-to-previous-virtual-line-n` were added.

#### java-mode

* A `java-mode` was added.
  
#### scala-mode

* A `scala-mode` was added.

#### dart-mode

* Added highlighting for strings, builtin functions, constants, keywords, operators and
  line comments.
  
#### vi-mode

* `Return` was added to visual mode.
* Now `*` searches symbol at point.
* Added support for search and replace using `%s///`.

#### scheme-mode

* Added error check to scheme slime function.
* Added `*use-scheme-process*` setting.
* Add loaded message for scheme-load-file.
* A scheme slime function to connect to `r7rs-swank`

#### js-mode

* Added an `eslint` command.
* Added a `prettier` to process whole buffer through [js prettier](https://prettier.io/) tool.

#### paredit

* Command `paredit-wrap` was added. It is bound to a `M-(` by default.

#### dot-mode

* Highting was added.

#### haskel-mode

* Highting was added.

#### ocaml-mode

* Highting was added.


### Changed

* `escape-delay` was made configurable.
* Refactored a number of functions:

  - `shortest-wait-timers` refactored and renamed to `get-next-timer-timing-ms`
  - `update-timer`
  - `read-key-1`

* All idle timers now kept in the `*processed-idle-timer-list*` list.
* Added different minibuffer classes:

  * `sticky-minibuffer-window`;
  * `popup-minibuffer-window` (is not fully supported yet).
  
  Both of them are inherited from a `floating-window` class.
  Function `make-minibuffer-window` creates a `sticky-minibuffer-window` object.

#### yaml-mode

* Now `yaml-mode` is autoenabled for `.yml` extension as well as for `.yaml`.

#### diff-mode

* Now `cl-ansi-text` system is used for coloring.
  
#### lisp-mode

* Changed swank protocol read message function to make it more reliable.
* Command `run-slime` now will ignore `*default-port*` variable and will
  always choose a random port.
* Now Lem will remember a lisp implementation you've chosen as default.
* Associate `*.lsp` with `lisp-mode`.

#### scheme-mode

* Now `C-x C-e` is bound to `scheme-eval-last-expression`.
* Added `*use-scheme-autodoc*` setting.

### Fixed

* String slurping in paredit-mode.
* Completion for filenames inside `"~"`.

  On SBCL Linux, `(pathname-directory "~/")` returns `(:absolute :home)`,
  which `completion-file` don't handle properly.
* A number of typos.
* Choosing an unique name for a buffer when file was saved by `write-file` command.
* Function `kill-ring-rotate` when `*kill-ring*` is empty.
* Command `revert-buffer` now keeps position of the cursor.
* Error raised when checking timers that have no ``last-time`` slot.
* Fixed cursor movement to the next line when it is at the end of line and width.
* Now LEM will try to presserve cursor position on scroll if possible.
* Now function `delete-process` will try to destroy a thread only if it is still alive.
* Command `filter-buffer` was fixed to not replace a buffer's content if command failed.

#### scheme-mode

* Fixed scheme-eval-region for scheme process.
* Fixed autodoc signature highlighting.

#### lem-pdcurses

* Resolve compiler warnings, etc.
* Fixed escape key input delaying.

#### nim-mode

* Nim multiline comments were fixed.

### lisp-mode

* Fixed an issue when connection to swank didn't initialized propertly.

  The root of the problem was that on OSX swank:*communication-style* is equal to :fd-handler
  for some reason. In this case swank:create-server does not start threads which
  accept connections on TCP port and process incoming messages.
  
### python-mode

* Command `run-python` was fixed for Windows.

  
### Thanks to

* Jéssica Milaré
* gos-k
* Ken OKADA
* cxxxr
* Hamayama
* FemtoEmacs
* Talen Bartlett

## [1.6] - 2019-08-29

This version and all previous are not covered by this changelog (yet?).
