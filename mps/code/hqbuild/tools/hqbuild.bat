@REM impl.bat.hqbuild: setup for SWIG autobuild system
@REM $Id$
@REM Copyright (c) 2001 Ravenbrook Limited.
@REM Called by SWIG autobuild system
@
@REM we expect whatcom to have set MSVCNT and possibly MSMASM and MSTOOLS
IF NOT %MSMASM%X == X SET PATH=%MSMASM%\bin;%PATH%
SET PATH=%MSVCNT%\..\sharedide\bin\ide;%MSVCNT%\..\sharedide\bin;%MSVCNT%\bin;%PATH%
SET INCLUDE=%MSVCNT%\include;%MSVCNT%\mfc\include;%INCLUDE%
SET LIB=%MSVCNT%\lib;%MSVCNT%\mfc\lib;%LIB%
@REM First argument is expected to be platform code, rest we pass on
nmake /f %1.nmk %2 %3 %4 %5 %6 %7 %8 %9
