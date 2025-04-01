
Thanks for contributing to Lem! Here are some guidelines that we follow in the codebase.

In general, we follow the Google guide: https://google.github.io/styleguide/lispguide.xml


## Alexandria and other utility libraries

Lem depends on `alexandria`, so you can use `if-let`, `when-let` and similar functions.

You can `:import-from` these symbols in your package definition, use the `alexandria:` package prefix, and define a package-local nickname.

Try to not use new utility functions that you don't see in the codebase yet.

For instance, we won't use `alexandria-2:line-up-first`.

Try to not use `alexandria:curry` and prefer higher-order functions.

## Dynamic bindings, functional style

Avoid dynamic symbol calls (`uiop:symbol-call`) but rething your architecture instead.

Use `defvar` and `defparameter` for user-facing variables, but avoid
using them as global variables that store state and that are used from
functions to functions. Have a more functional style, give explicit
arguments to functions.

Example:

```lisp
;; avoid this
(defvar *var* 1)
(defun foo ()
   *var*)
(let ((*var* 2))
    (foo))
```

instead, have `foo` take one argument.


## Don't ignore compiler warnings

Please take attention to compiler warnings.


## Deprecation warnings

If you change or delete a feature, if only a variable name, you must
take care of deprecation warnings. A user should be notified that
something changed. If possible, her old config file should not fail
loading, or it should not fail without notice. Measures can vary. Ideas:

- don't delete or rename a `defvar`, a `defparameter` or a function, but leave it and if it is used, signal a warning or an error. Add a `;; DEPRECATED` comment with the date of the comment.
- document the breaking change on the "next release changelog" issue (like https://github.com/lem-project/lem/issues/1027 or equivalent).


## Documentation

Write thorough docstrings to interactive commands (`define-command`),
give meaningful documentation to important functions, give a
high-level overview in packages and comments (the "why", not the
"how"). Use the `:documentation` option of packages, generic functions
and CLOS slots.

Please also contribute to Lem's website if you add or change a feature.

https://github.com/lem-project/lem-project.github.io/

or at least, open an issue about it so we don't forget to do it, thank you.


## Errors in Lem

`error` is for internal errors, and `editor-error` is displayed nicely to the user.

## File layout

Variables and parameters (`defvar`, `defparameter`) should be grouped
and appear near the top of the file, before conditions, classes and
functions.

### Major and minor modes keybindings layout

Lem modes should define all their keybindings at the top of the file.

## Git and pull requests

Please rebase and squash small commits together (you can do this with lem/legit ! ;) ).

When your changes are about a mode or a feature, we like the commit message to say it upfront, for example:

    legit: add k to discard changes of unstaged files


## Loop

Loop keywords are written as keywords, with the `:`:

```lisp
(loop :for key :in … )
```

## Macros, backquote, comma

Don't write long macros, use the "call-with-" pattern.

Don't define lists with backquote and comma, use the `list` constructor.


## User-visible variables names

Parameters that can be changed by the user should not have a "-p"
suffix. Keep them for the functions' key arguments.

They can be saved and set-able with the `lem:config` system.
