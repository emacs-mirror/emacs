@REM impl.bat.hqbuild
@REM $HopeName: MMsrc!hqbuild:tools:hqbuild.bat(trunk.3) $
@REM Copyright (C) 1996,1997 Harlequin Group, all rights reserved
@REM Called by SWIG autobuild system
@
@REM we expect whatcom to have set MSVCNT and possibly MSMASM
IF NOT %MSMASM%X == X SET PATH=%MSMASM%\bin;%PATH%
SET PATH=%MSVCNT%\bin;%PATH%
SET LIB=%MSVCNT%\lib
SET INCLUDE=%MSVCNT%\include
@REM First argument is expected to be platform code, rest we pass on
nmake /f %1.nmk %2 %3 %4 %5 %6 %7 %8 %9
