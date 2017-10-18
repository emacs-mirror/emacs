$ ! VMS command file to make the definitions needed to run the installed Emacs.
$ ! You must edit the file names herein when you install Emacs.
$ ! You must execute this in each session in order to run Emacs
$ !  or else it must be executed by the system at each boot.
$ ! To execute at boot time, specify "/SYSTEM" as the first parameter.
$
$ ! The following line must be changed according to where
$ ! in the file system you install Emacs.
$
$ define 'p1' /translation=concealed emacs_library	sys$sysdevice:[emacs.]
$
$ ! These should no longer be needed, because everything
$ ! is now written to use emacs_library.
$ ! define 'p1' emacs_lisplib  emacs_library:[lisp]
$ ! define 'p1' emacs_etc      emacs_library:[etc]
$ ! define 'p1' emacs_lock     emacs_library:[lock]
$ ! define 'p1' termcap	emacs_library:[etc]termcap.dat
$ runemacs :== $emacs_library:[000000]emacs -map emacs_library:[000000]emacs.dump
$ emacs :== @emacs_library:[000000]kepteditor emacs
