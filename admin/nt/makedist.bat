@echo off

rem This batch file doesn't work with Cygwin tar because #files#
rem has DOS line endings, which Cygwin tar misinterprets.
rem I use the version of tar from
rem    ftp://ftp.gnu.org/gnu/windows/emacs/utilities/i386/tar-1.11.2a.exe
rem renamed as wtar.exe.
set TAR=wtar

rem Make a copy of current Emacs source
if (%3) == () goto usage
if not (%4) == () goto %4
if not (%4) == (src) goto :lisp

:src

echo Create full source distribution, excluding leim
%TAR%  --exclude leim --exclude _marker --exclude DOC --exclude DOC-X --exclude TAGS --exclude bin --exclude obj --exclude obj-spd --exclude oo --exclude oo-spd --exclude *~ --exclude *.rej -cvf - emacs-%1 | gzip -9 > %2-src.tar.gz
if not (%4) == () goto end

:lisp
echo Create limited elisp source distribution
%TAR% --exclude *.rej --exclude *.elc --exclude *~ -cvf - emacs-%1/lisp | gzip -9 > %2-lisp.tar.gz
if not (%4) == () goto end

:bin

set eld=emacs-%1/lisp

rem Keep this list in sync with the DONTCOMPILE list in lisp/Makefile.in

set elfiles=%eld%/cus-load.el %eld%/cus-start.el %eld%/emacs-lisp/cl-specs.el %eld%/eshell/esh-maint.el %eld%/eshell/esh-groups.el %eld%/finder-inf.el %eld%/forms-d2.el %eld%/forms-pass.el %eld%/generic-x.el %eld%/international/latin-1.el %eld%/international/latin-2.el %eld%/international/latin-3.el %eld%/international/latin-4.el %eld%/international/latin-5.el %eld%/international/latin-8.el %eld%/international/latin-9.el %eld%/international/mule-conf.el %eld%/loaddefs.el %eld%/loadup.el %eld%/mail/blessmail.el %eld%/patcomp.el %eld%/paths.el %eld%/play/bruce.el %eld%/subdirs.el %eld%/version.el

set fns_el=
for %%f in (emacs-%1/bin/fns*) do set fns_el=%fns_el% emacs-%1/bin/%%f

echo Create bin distribution
copy %3\README.W32 emacs-%1\README.W32

del #files#
for %%f in (emacs-%1/BUGS emacs-%1/README emacs-%1/README.W32) do echo %%f>>#files#
for %%f in (emacs-%1/bin/fns*) do echo emacs-%1/bin/%%f>>#files#
for %%f in (emacs-%1/bin emacs-%1/etc emacs-%1/info emacs-%1/lisp %elfiles%) do echo %%f>>#files#
for %%f in (%eld%/term/*.el) do echo %eld%/term/%%f>>#files#
for %%f in (emacs-%1/lock emacs-%1/site-lisp emacs-%1/site-lisp/subdirs.el) do echo %%f>>#files#
%TAR% --exclude temacs.exe --exclude emacs.mdp --exclude *.pdb --exclude *.opt --exclude *.el --exclude *~ -T #files# -cvf - | gzip -9 > %2-bin-i386.tar.gz
del emacs-%1\README.W32
del #files#
if not (%4) == () goto end

:fullbin

echo Create full bin distribution
copy %3\README.W32 emacs-%1\README.W32

%TAR% --exclude temacs.exe --exclude emacs.mdp --exclude *.pdb --exclude *.opt --exclude *~ -cvf - emacs-%1/BUGS emacs-%1/README emacs-%1/README.W32 emacs-%1/bin emacs-%1/etc emacs-%1/info emacs-%1/lisp emacs-%1/lock emacs-%1/site-lisp | gzip -9 > %2-fullbin-i386.tar.gz
del emacs-%1\README.W32
if not (%4) == () goto end

:leim

echo Create archive with precompiled leim files
%TAR% -cvf - emacs-%1/leim/leim-list.el emacs-%1/leim/quail emacs-%1/leim/ja-dic | gzip -9 > %2-leim.tar.gz
if not (%4) == () goto end

:undumped

echo Create archive with extra files needed for redumping emacs
copy %3\README-UNDUMP.W32 emacs-%1\README-UNDUMP.W32
copy %3\dump.bat emacs-%1\bin
if exist emacs-%1\src\obj-spd\i386\temacs.exe copy emacs-%1\src\obj-spd\i386\temacs.exe emacs-%1\bin
if exist emacs-%1\src\oo-spd\i386\temacs.exe copy emacs-%1\src\oo-spd\i386\temacs.exe emacs-%1\bin
%TAR% -cvf - emacs-%1/README-UNDUMP.W32 emacs-%1/bin/dump.bat emacs-%1/bin/temacs.exe | gzip -9 > %2-undumped-i386.tar.gz
del emacs-%1\bin\temacs.exe
del emacs-%1\bin\dump.bat
del emacs-%1\README-UNDUMP.W32
if not (%4) == () goto end

:barebin

echo Create archive with just the basic binaries and generated files
echo (the user needs to unpack the full source distribution for
echo  everything else)
copy %3\README.W32 emacs-%1\README.W32
%TAR% -cvf - emacs-%1/README.W32 emacs-%1/bin emacs-%1/etc/DOC emacs-%1/etc/DOC-X | gzip -9 > %2-barebin-i386.tar.gz
del emacs-%1\README.W32
if not (%4) == () goto end

goto end

rem Only do this if explicitly requested
:zipfiles

echo Create zip files for bin and lisp archives
mkdir distrib
cd distrib
gunzip -c ..\%2-bin-i386.tar.gz | %TAR% xf -
rem Need to split emacs.exe into fragments because it is too big now
rem to fit on a floppy even by itself.
copy %3\stitch.bat %2\bin
cd %2\bin
split -b 1000000 emacs.exe emacs
del emacs.exe
cd ..\..
zip -rp9 em%5bin %2
rm -rf %2
zipsplit -n 1400000 -b .. em%5bin.zip
del em%5bin.zip
gunzip -c ..\%2-lisp.tar.gz | %TAR% xf -
zip -rp9 em%5lis %2
rm -rf %2
zipsplit -n 1400000 -b .. em%5lis.zip
del em%5lis.zip
cd ..

goto end

:usage
echo Generate source and binary distributions of emacs.
echo Usage: %0 emacs-version dist-basename distfiles [lisp,bin,undumped,barebin]
echo   (e.g., %0 19.34 emacs-19.34.5 d:\andrewi\distfiles)
echo Or: %0 emacs-version dist-basename distfiles "zipfiles" short-version
echo   (e.g., %0 20.6 emacs-20.6 d:\andrewi\distfiles zipfiles 206)
:end

goto skipArchTag
   arch-tag: 6e2ddd92-c1c9-4992-b6b5-207aaab72f68
:skipArchTag
