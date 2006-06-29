REM Batch file to build and run example code
REM $Id$
REM /NODEFAULTLIB:library

cl /I..\..\code hwgc01.c /Fohwgc01 ..\..\code\w3i3mv\ci\mps.lib ..\..\code\w3i3mv\ci\mpsplan.lib

hwgc01.exe


cl /I..\..\code hwgc02.c /Fohwgc02 ..\..\code\w3i3mv\ci\mps.lib ..\..\code\w3i3mv\ci\mpsplan.lib ..\..\code\w3i3mv\ci\fmtno.obj

hwgc02.exe
