# FBAR

FBAR is a minimal file-tree-bar for Lem editor.  

FBAR permits a fast and easy, as well as a persistent, listing of files and directories, in a pop-up window on the left side of the screen.

## Quickstart

Load FBAR from your `~/.lem/init.lisp` file with something like:
```
(load-library "fbar")
(lem-fbar:fbar (truename "~/path/to/projects/"))
```

A new keybinding `<C-x f>` is added to trigger the fbar; `<Escape>` cancels it.  

`<Up>`, `<Down>` as well as vi `<j>` and `<k>` keys navigate up and down.  Hitting `<Return>` on a directory opens (or closes) it; on a file `<Return>` attempts to load the file.
