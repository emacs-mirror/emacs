$ ! VMS command file to run `temacs.exe' and dump the data file `temacs.dump'.
$ if f$search("temacs.dump;") .nes. "" then delete temacs.dump;*
$ temacs :== $emacs_library:[src]temacs -batch
$ temacs -l inc-vers
$ temacs -l loadup.el dump
