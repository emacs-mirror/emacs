$ set noverify
$ ! VMS command file to recompile all .C files which need recompilation.
$ ! These are all .C files that have no .OBJ files or that are newer
$ ! than the corresponding .OBJ files.  This file is self contained
$ ! and does not require you to do anything before running it.
$ !***
$ !***  Mukesh Prasad		compile.com	Nov. 18, 1985
$ !***
$ !***	Compile all .C files in the directory 'sourcedir'.
$ !***  If an argument is present, compiles all files, otherwise
$ !***  compiles only files modified since last compile.
$ !***
$
$! First we try to sense which C compiler we have available.
$!
$set noon		!do not bomb if an error occurs.
$assign nla0: sys$output
$assign nla0: sys$error  !so we do not get an error message about this.
$cc nla0:compiler_check.c
$if $status.eq.%x38090 then goto try_gcc
$goto have_compiler
$!
$try_gcc:
$gcc nla0:compiler_check.c
$if $status.eq.%x38090 then goto whoops
$ CC :== GCC
$goto have_compiler
$!
$whoops:
$write sys$output "You must have a C compiler to build Emacs.  Sorry."
$deassign sys$output
$deassign sys$error
$exit %x38090
$!
$!
$have_compiler:
$deassign sys$output
$deassign sys$error
$set on
$if f$search("compiler_check.obj").nes."" then dele/nolog compiler_check.obj;
$write sys$output "Building Emacs with the ''cc' compiler."
$
$    @precomp
$    @recomp dispnew.c
$    @recomp scroll.c
$    @recomp xdisp.c
$    @recomp window.c
$    @recomp term.c
$    @recomp cm.c
$    @recomp emacs.c
$    @recomp keyboard.c
$    @recomp macros.c
$    @recomp keymap.c
$    @recomp sysdep.c
$    @recomp buffer.c
$    @recomp filelock.c
$    @recomp insdel.c
$    @recomp marker.c
$    @recomp minibuf.c
$    @recomp fileio.c
$    @recomp dired.c
$    @recomp filemode.c
$    @recomp cmds.c
$    @recomp casefiddle.c
$    @recomp indent.c
$    @recomp search.c
$    @recomp regex.c
$    @recomp undo.c
$    @recomp alloc.c
$    @recomp data.c
$    @recomp doc.c
$    @recomp editfns.c
$    @recomp callint.c
$    @recomp eval.c
$    @recomp fns.c
$    @recomp print.c
$    @recomp lread.c
$    @recomp abbrev.c
$    @recomp syntax.c
$    @recomp mocklisp.c
$    @recomp bytecode.c
$    @recomp process.c
$    @recomp callproc.c
$    @recomp vmsfns.c
$    @recomp doprnt.c
$    @recomp vmsmap.c
$    @recomp termcap.c
$    @recomp tparam.c
$    @recomp lastfile.c
$    @recomp malloc.c
$    @recomp alloca.c
