@rem impl.bat.proddw
@rem Script that automates building and collating a dylan product
rem $HopeName: MMsrc!proddw.bat(trunk.5) $
nmake /f w3i3mv.nmk VARIETY=ci mmdw.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=hi mmdw.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=he mmdw.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=wi mmdw.lib mpsplan.lib
rmdir /Q/S dylan
mkdir dylan
mkdir dylan\mps
mkdir dylan\mps\include
mkdir dylan\mps\lib
mkdir dylan\mps\lib\w3i3
mkdir dylan\mps\lib\w3i3\ci
mkdir dylan\mps\lib\w3i3\hi
mkdir dylan\mps\lib\w3i3\he
mkdir dylan\mps\lib\w3i3\wi
mkdir dylan\mps\src
copy mps.h dylan\mps\include
copy mpsavm.h dylan\mps\include
copy mpscamc.h dylan\mps\include
copy mpscawl.h dylan\mps\include
copy mpsclo.h dylan\mps\include
copy mpscsnc.h dylan\mps\include
copy mpscmv.h dylan\mps\include
copy mpsio.h dylan\mps\include
copy mpslib.h dylan\mps\include
copy mpstd.h dylan\mps\include
copy mpsw3.h dylan\mps\include
copy mpswin.h dylan\mps\include
copy w3i3mv\ci\mmdw.lib dylan\mps\lib\w3i3\ci
copy w3i3mv\hi\mmdw.lib dylan\mps\lib\w3i3\hi
copy w3i3mv\he\mmdw.lib dylan\mps\lib\w3i3\he
copy w3i3mv\wi\mmdw.lib dylan\mps\lib\w3i3\wi
copy w3i3mv\ci\mpsplan.lib dylan\mps\lib\w3i3\ci
copy w3i3mv\hi\mpsplan.lib dylan\mps\lib\w3i3\hi
copy w3i3mv\he\mpsplan.lib dylan\mps\lib\w3i3\he
copy w3i3mv\wi\mpsplan.lib dylan\mps\lib\w3i3\wi
copy mpsliban.c dylan\mps\src
copy mpsioan.c dylan\mps\src
