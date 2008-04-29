@rem impl.bat.gathconf: GATHERING A RELEASE FOR CONFIGURA
@rem
@rem $HopeName: !gathconf.bat(trunk.1) $
@rem $Id$
@rem Copyright (C) 2000 Harlequin Limited.  All rights reserved.
@rem Copyright (C) 2005-2007 Ravenbrook Limited.  All rights reserved.


@set mpsreleasename=exp-164845


rmdir /q/s w3i3mv
nmake /f w3i3mv.nmk VARIETY=we mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3mv.nmk VARIETY=hi mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3mv.nmk VARIETY=di mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3mv.nmk VARIETY=ci mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll

rmdir /q/s %mpsreleasename%
mkdir %mpsreleasename%

mkdir %mpsreleasename%\include
copy mps.h %mpsreleasename%\include
copy mpsavm.h %mpsreleasename%\include
copy mpsacl.h %mpsreleasename%\include
copy mpscamc.h %mpsreleasename%\include
copy mpscams.h %mpsreleasename%\include
copy mpscawl.h %mpsreleasename%\include
copy mpsclo.h %mpsreleasename%\include
copy mpscmv.h %mpsreleasename%\include
copy mpscmvff.h %mpsreleasename%\include
copy mpscsnc.h %mpsreleasename%\include
copy mpsio.h %mpsreleasename%\include
copy mpslib.h %mpsreleasename%\include
copy mpslibcb.h %mpsreleasename%\include
copy mpstd.h %mpsreleasename%\include
copy mpsw3.h %mpsreleasename%\include
copy mpswin.h %mpsreleasename%\include

mkdir %mpsreleasename%\lib
mkdir %mpsreleasename%\lib\w3i3

mkdir %mpsreleasename%\lib\w3i3\we
@echo we-%mpsreleasename% > %mpsreleasename%\lib\w3i3\we\we-%mpsreleasename%.txt
copy w3i3mv\we\mps.lib %mpsreleasename%\lib\w3i3\we
copy w3i3mv\we\mpsplan.lib %mpsreleasename%\lib\w3i3\we
copy w3i3mv\we\mpsplcb.lib %mpsreleasename%\lib\w3i3\we
copy w3i3mv\we\mpsdy.dll %mpsreleasename%\lib\w3i3\we
copy w3i3mv\we\mpsdy.lib %mpsreleasename%\lib\w3i3\we

mkdir %mpsreleasename%\lib\w3i3\hi
@echo hi-%mpsreleasename% > %mpsreleasename%\lib\w3i3\hi\hi-%mpsreleasename%.txt
copy w3i3mv\hi\mps.lib %mpsreleasename%\lib\w3i3\hi
copy w3i3mv\hi\mpsplan.lib %mpsreleasename%\lib\w3i3\hi
copy w3i3mv\hi\mpsplcb.lib %mpsreleasename%\lib\w3i3\hi
copy w3i3mv\hi\mpsdy.dll %mpsreleasename%\lib\w3i3\hi
copy w3i3mv\hi\mpsdy.lib %mpsreleasename%\lib\w3i3\hi

mkdir %mpsreleasename%\lib\w3i3\di
@echo di-%mpsreleasename% > %mpsreleasename%\lib\w3i3\di\di-%mpsreleasename%.txt
copy w3i3mv\di\mps.lib %mpsreleasename%\lib\w3i3\di
copy w3i3mv\di\mpsplan.lib %mpsreleasename%\lib\w3i3\di
copy w3i3mv\di\mpsplcb.lib %mpsreleasename%\lib\w3i3\di
copy w3i3mv\di\mpsdy.dll %mpsreleasename%\lib\w3i3\di
copy w3i3mv\di\mpsdy.lib %mpsreleasename%\lib\w3i3\di

mkdir %mpsreleasename%\lib\w3i3\ci
@echo ci-%mpsreleasename% > %mpsreleasename%\lib\w3i3\ci\ci-%mpsreleasename%.txt
copy w3i3mv\ci\mps.lib %mpsreleasename%\lib\w3i3\ci
copy w3i3mv\ci\mpsplan.lib %mpsreleasename%\lib\w3i3\ci
copy w3i3mv\ci\mpsplcb.lib %mpsreleasename%\lib\w3i3\ci
copy w3i3mv\ci\mpsdy.dll %mpsreleasename%\lib\w3i3\ci
copy w3i3mv\ci\mpsdy.lib %mpsreleasename%\lib\w3i3\ci

mkdir %mpsreleasename%\src
copy mpsliban.c %mpsreleasename%\src
copy mpsioan.c %mpsreleasename%\src
