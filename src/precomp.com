$ ! VMS command file to define the `ccom' command
$ ! that is the only way to compile Emacs C source files.
$
$	define /nolog vaxc$include sys$library, sys$disk:[]
$	define /nolog sys vaxc$include
$  ccom :== @ccom
