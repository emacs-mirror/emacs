@REM impl.bat.hqbuild
@REM $HopeName: MMsrc!tools:swbuild.bat(trunk.1) $
@REM Copyright (C) 1996 Harlequin Group, all rights reserved
@REM Called by SWIG autobuild system
@
@REM Silently throw away all but first argument
nmake /f %1.nmk
