@REM impl.bat.hqbuild
<<<<<<< /u/nickb/mm/merge/trunk/src/hqbuild:tools:hqbuild.bat
@REM $HopeName: MMsrc!hqbuild:tools:hqbuild.bat(trunk.2) $
=======
@REM $HopeName: MMsrc!hqbuild:tools:hqbuild.bat(MMdevel_sw_eq.2) $
>>>>>>> 1.2.5.2
@REM Copyright (C) 1996 Harlequin Group, all rights reserved
@REM Called by SWIG autobuild system
@
@REM we expect whatcom to have set MSVCNT and possibly MSMASM
IF NOT %MSMASM%X == X SET PATH=%MSMASM%\bin;%PATH%
SET PATH=%MSVCNT%\bin;%PATH%
SET LIB=%MSVCNT%\lib
SET INCLUDE=%MSVCNT%\include
@REM Silently throw away all but first argument
nmake /f %1.nmk %2 %3 %4 %5 %6 %7 %8 %9
