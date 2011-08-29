@ECHO OFF
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM $Id$
REM $Change$  $DateTime$  $Author$
REM
REM Configure access to the Microsoft SDK.
REM
REM This procedure has only been tested under NT6.1.  Earlier versions
REM of the operating system will have different SDK configuration
REM details.
REM
REM This procedure requires delayed environment expansion.
REM The presence of REG in the command path is assumed.
REM
REM The registry is queried to determine the SDK installation location
REM and its SETENV command procedure is invoked to configure it.
REM Note that SETENV, as installed, has a tendency to alter the color
REM settings for the console.
REM
REM Copyright (C) 2011 Ravenbrook Limited.  All rights reserved.
REM
REM -------------------------------------------------------------------
REM
SET WinSDKRegKeyPath=HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SDKs\Windows\v7.1

FOR /F "tokens=2*" %%A IN ('REG QUERY "%WinSDKRegKeyPath%" /v InstallationFolder') DO SET WindowsSDKDir=%%B

SET "sdkdir=%WindowsSDKDir%"

%sdkdir%\bin\setenv

REM -- End of program

