@rem impl.bat.gathconf: GATHERING A RELEASE FOR CONFIGURA
@rem
@rem $HopeName: !gathconf.bat(trunk.1) $
@rem $Id$
@rem Copyright (C) 2000 Harlequin Limited.  All rights reserved.
@rem Copyright (C) 2005-2006 Ravenbrook Limited.  All rights reserved.

rmdir /q/s w3i3mv
nmake /f w3i3mv.nmk VARIETY=we mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3mv.nmk VARIETY=hi mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3mv.nmk VARIETY=di mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3mv.nmk VARIETY=ci mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
mkdir release

mkdir release\include
copy mps.h release\include
copy mpsavm.h release\include
copy mpsacl.h release\include
copy mpscamc.h release\include
copy mpscams.h release\include
copy mpscawl.h release\include
copy mpsclo.h release\include
copy mpscmv.h release\include
copy mpscmvff.h release\include
copy mpscsnc.h release\include
copy mpsio.h release\include
copy mpslib.h release\include
copy mpslibcb.h release\include
copy mpstd.h release\include
copy mpsw3.h release\include
copy mpswin.h release\include

mkdir release\lib
mkdir release\lib\w3i3

mkdir release\lib\w3i3\we
copy w3i3mv\we\mps.lib release\lib\w3i3\we
copy w3i3mv\we\mpsplan.lib release\lib\w3i3\we
copy w3i3mv\we\mpsplcb.lib release\lib\w3i3\we
copy w3i3mv\we\mpsdy.dll release\lib\w3i3\we
copy w3i3mv\we\mpsdy.lib release\lib\w3i3\we

mkdir release\lib\w3i3\hi
copy w3i3mv\hi\mps.lib release\lib\w3i3\hi
copy w3i3mv\hi\mpsplan.lib release\lib\w3i3\hi
copy w3i3mv\hi\mpsplcb.lib release\lib\w3i3\hi
copy w3i3mv\hi\mpsdy.dll release\lib\w3i3\hi
copy w3i3mv\hi\mpsdy.lib release\lib\w3i3\hi

mkdir release\lib\w3i3\di
copy w3i3mv\di\mps.lib release\lib\w3i3\di
copy w3i3mv\di\mpsplan.lib release\lib\w3i3\di
copy w3i3mv\di\mpsplcb.lib release\lib\w3i3\di
copy w3i3mv\di\mpsdy.dll release\lib\w3i3\di
copy w3i3mv\di\mpsdy.lib release\lib\w3i3\di

mkdir release\lib\w3i3\ci
copy w3i3mv\ci\mps.lib release\lib\w3i3\ci
copy w3i3mv\ci\mpsplan.lib release\lib\w3i3\ci
copy w3i3mv\ci\mpsplcb.lib release\lib\w3i3\ci
copy w3i3mv\ci\mpsdy.dll release\lib\w3i3\ci
copy w3i3mv\ci\mpsdy.lib release\lib\w3i3\ci

mkdir release\src
copy mpsliban.c release\src
copy mpsioan.c release\src
