@echo off
set emacs_dir=c:\emacs
prep /om /ft %emacs_dir%\src\obj\i386\emacs
if errorlevel 1 goto done
profile %emacs_dir%\src\obj\i386\emacs %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto done
prep /m  %emacs_dir%\src\obj\i386\emacs
if errorlevel 1 goto done
plist  %emacs_dir%\src\obj\i386\emacs > info/emacs.prof
:done
