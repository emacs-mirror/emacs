## What is it?

This is Emacs' master branch, with Common Lisp packages added (and
some stuff removed, which I was too lazy to support).

I am using this on a daily basis on MacOS with an init file that I can
also use with an unchanged master. I make no efforts to find or fix
bugs in stuff that I don't use personally.

My init file uses `use-package` and `straight`, and I am using
packages like `vertico`, `consult`, `corfu`, `magit`, `org`, so I'd
say it's quite backwards compatible.

## Who's that?

`C-h i d m emacs RET s gerd RET`

## What's not there/not supported

* Documentation
* Tests
* Support for pure space
* Support for symbol shortcuts

## What's there

You can use packages much like in Common Lisp. For details, read
`lisp/emacs-lisp/pkg.el` and `src/pkg.c`, and maybe
`admin/cl-packages.org`, although I haven't kept that up-to-date for
some time, I guess.

## What's the plan?

None. I don't think CL packages will land in Emacs in my lifetime.
I'd say the resistance against anything CL is simply too high among
current Emacs maintainers, especially rms.

So, why do this?  Because I can. Some people like to tinker with their
init files, others go a step further...
