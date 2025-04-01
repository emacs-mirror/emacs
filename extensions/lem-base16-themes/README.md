# base16 themes for Lem editor/IDE

## Installation

To use base16 themes in Lem editor run

```
$ ros install lukpank/lem-base16-themes
```

then add to your `~/.lem/init.lisp` the following lines

```
(load-library "base16-themes")
(load-theme "espresso")
```

changing the theme `espresso` to the theme you want to use.

You can also change the theme interactively with `M-x load-theme`.

## Regenerating themes

To regenerate `src/themes.lisp` run in slime REPL

```
(load "/path/to/lem-base16-themes/lem-base16-themes-generate.asd")
(ql:quickload "lem-base16-themes-generate")
(lem-base16-themes-generate:regenerate-themes)
```

## License

Licensed under [MIT License](https://github.com/lukpank/lem-base16-themes/blob/master/LICENSE).

## Author

Łukasz Pankowski <lukpank at o2 dot pl>
