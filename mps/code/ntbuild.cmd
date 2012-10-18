@ECHO OFF
REM ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM $Id$
REM $Change$  $DateTime$  $Author$
REM
REM Build the MPS and construct a kit.
REM Copyright (C) 2011 Ravenbrook Limited.  All rights reserved.
REM
REM P1 is the name of the release, which should match the name
REM in the version.c source file.
REM The current directory is assumed to be the code directory.
REM
REM --------------------------------------------------------------------

SETLOCAL

SET mpsreleasename=%1
IF NOT DEFINED mpsreleasename GOTO :error

REM
REM The make file is designed to construct the target files under
REM a w3i3mv subdirectory of the current directory.  We remove this
REM if it already exists, so that the target is guaranteed to be clean.
REM

IF EXIST w3i3mv\NUL RMDIR /Q/S w3i3mv

REM Make four varieties of target file.  If there are build errors,
REM abort as soon as possible.  Any non zero return code from nmake
REM indicates a problem.
REM See http://msdn.microsoft.com/en-us/library/afyyse50(v=VS.71).aspx

ECHO * Building four varieties into the w3i3mv subdirectory...

NMAKE /F w3i3mv.nmk VARIETY=we mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
IF %ERRORLEVEL% GTR 0 GOTO :build_error
NMAKE /F w3i3mv.nmk VARIETY=hi mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
IF %ERRORLEVEL% GTR 0 GOTO :build_error
NMAKE /F w3i3mv.nmk VARIETY=di mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
IF %ERRORLEVEL% GTR 0 GOTO :build_error
NMAKE /F w3i3mv.nmk VARIETY=ci mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
IF %ERRORLEVEL% GTR 0 GOTO :build_error

REM Now purge and construct the kit release subdirectory.

ECHO Populating the %mpsreleasename% subdirectory...

IF EXIST %mpsreleasename%\NUL RMDIR /Q/S %mpsreleasename%
MKDIR %mpsreleasename%\include

COPY mps.h %mpsreleasename%\include
COPY mpsavm.h %mpsreleasename%\include
COPY mpsacl.h %mpsreleasename%\include
COPY mpscamc.h %mpsreleasename%\include
COPY mpscams.h %mpsreleasename%\include
COPY mpscawl.h %mpsreleasename%\include
COPY mpsclo.h %mpsreleasename%\include
COPY mpscmv.h %mpsreleasename%\include
COPY mpscmvff.h %mpsreleasename%\include
COPY mpscsnc.h %mpsreleasename%\include
COPY mpsio.h %mpsreleasename%\include
COPY mpslib.h %mpsreleasename%\include
COPY mpslibcb.h %mpsreleasename%\include
COPY mpstd.h %mpsreleasename%\include
COPY mpsw3.h %mpsreleasename%\include
COPY mpswin.h %mpsreleasename%\include

MKDIR %mpsreleasename%\lib
COPY w3gen.def %mpsreleasename%\lib\mps-fns.def

MKDIR %mpsreleasename%\lib\W3I3MV

MKDIR %mpsreleasename%\lib\W3I3MV\we
ECHO we-%mpsreleasename% > %mpsreleasename%\lib\W3I3MV\we\we-%mpsreleasename%.txt
COPY w3i3mv\we\mps.lib %mpsreleasename%\lib\W3I3MV\we
COPY w3i3mv\we\mpsplan.lib %mpsreleasename%\lib\W3I3MV\we
COPY w3i3mv\we\mpsplcb.lib %mpsreleasename%\lib\W3I3MV\we
COPY w3i3mv\we\mpsdy.dll %mpsreleasename%\lib\W3I3MV\we
COPY w3i3mv\we\mpsdy.lib %mpsreleasename%\lib\W3I3MV\we

MKDIR %mpsreleasename%\lib\W3I3MV\hi
ECHO hi-%mpsreleasename% > %mpsreleasename%\lib\W3I3MV\hi\hi-%mpsreleasename%.txt
COPY w3i3mv\hi\mps.lib %mpsreleasename%\lib\W3I3MV\hi
COPY w3i3mv\hi\mpsplan.lib %mpsreleasename%\lib\W3I3MV\hi
COPY w3i3mv\hi\mpsplcb.lib %mpsreleasename%\lib\W3I3MV\hi
COPY w3i3mv\hi\mpsdy.dll %mpsreleasename%\lib\W3I3MV\hi
COPY w3i3mv\hi\mpsdy.lib %mpsreleasename%\lib\W3I3MV\hi

MKDIR %mpsreleasename%\lib\W3I3MV\di
ECHO di-%mpsreleasename% > %mpsreleasename%\lib\W3I3MV\di\di-%mpsreleasename%.txt
COPY w3i3mv\di\mps.lib %mpsreleasename%\lib\W3I3MV\di
COPY w3i3mv\di\mpsplan.lib %mpsreleasename%\lib\W3I3MV\di
COPY w3i3mv\di\mpsplcb.lib %mpsreleasename%\lib\W3I3MV\di
COPY w3i3mv\di\mpsdy.dll %mpsreleasename%\lib\W3I3MV\di
COPY w3i3mv\di\mpsdy.lib %mpsreleasename%\lib\W3I3MV\di

MKDIR %mpsreleasename%\lib\W3I3MV\ci
ECHO ci-%mpsreleasename% > %mpsreleasename%\lib\W3I3MV\ci\ci-%mpsreleasename%.txt
COPY w3i3mv\ci\mps.lib %mpsreleasename%\lib\W3I3MV\ci
COPY w3i3mv\ci\mpsplan.lib %mpsreleasename%\lib\W3I3MV\ci
COPY w3i3mv\ci\mpsplcb.lib %mpsreleasename%\lib\W3I3MV\ci
COPY w3i3mv\ci\mpsdy.dll %mpsreleasename%\lib\W3I3MV\ci
COPY w3i3mv\ci\mpsdy.lib %mpsreleasename%\lib\W3I3MV\ci

MKDIR %mpsreleasename%\src
COPY mpsliban.c %mpsreleasename%\src
COPY mpsioan.c %mpsreleasename%\src

ECHO * NT build complete
GOTO :end

:ERROR
ECHO * Error: You must provide a release name as the first parameter.
GOTO :end

:BUILD_ERROR
ECHO * An error occurred during the build process.
GOTO :end


:END
ENDLOCAL
