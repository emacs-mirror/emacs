
set emacs_path=\emacs

@echo off

set EMACSLOADPATH=%emacs_path%\lisp
set SHELL=cmd
set EMACSDATA=%emacs_path%\etc
set EMACSPATH=%emacs_path%\bin
set EMACSLOCKDIR=%emacs_path%\lock
set INFOPATH=%emacs_path%\info
set EMACSDOC=%emacs_path%\etc
set TERM=CMD

rem To find the ~\_emacs file
set HOME=%emacs_path%\nt

%emacs_path%\bin\emacs.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
