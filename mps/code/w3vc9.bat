@rem impl.bat.gathconf: GATHERING A RELEASE FOR CONFIGURA
@rem
@rem $HopeName: !gathconf.bat(trunk.1) $
@rem $Id$
@rem Copyright (C) 2000 Harlequin Limited.  All rights reserved.
@rem Copyright (C) 2005-2008 Ravenbrook Limited.  All rights reserved.


IF DEFINED mpsreleasename (
@echo "Adding vc9 build into mpsreleasename=%mpsreleasename%"
) ELSE (
@echo "Error: mpsreleasename is not defined."
@echo "You must run w3build.bat first, then run VC9's vcvars32.bat, then run this script."
exit /b
)

rmdir /q/s w3i3m9
nmake /f w3i3m9.nmk VARIETY=we mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3m9.nmk VARIETY=hi mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3m9.nmk VARIETY=di mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll
nmake /f w3i3m9.nmk VARIETY=ci mps.lib mpsplan.lib mpsplcb.lib mpsdy.dll

mkdir %mpsreleasename%\lib\w3i3m9

mkdir %mpsreleasename%\lib\w3i3m9\we
@echo we-%mpsreleasename% > %mpsreleasename%\lib\w3i3m9\we\we-%mpsreleasename%.txt
copy w3i3m9\we\mps.lib %mpsreleasename%\lib\w3i3m9\we
copy w3i3m9\we\mpsplan.lib %mpsreleasename%\lib\w3i3m9\we
copy w3i3m9\we\mpsplcb.lib %mpsreleasename%\lib\w3i3m9\we
copy w3i3m9\we\mpsdy.dll %mpsreleasename%\lib\w3i3m9\we
copy w3i3m9\we\mpsdy.lib %mpsreleasename%\lib\w3i3m9\we

mkdir %mpsreleasename%\lib\w3i3m9\hi
@echo hi-%mpsreleasename% > %mpsreleasename%\lib\w3i3m9\hi\hi-%mpsreleasename%.txt
copy w3i3m9\hi\mps.lib %mpsreleasename%\lib\w3i3m9\hi
copy w3i3m9\hi\mpsplan.lib %mpsreleasename%\lib\w3i3m9\hi
copy w3i3m9\hi\mpsplcb.lib %mpsreleasename%\lib\w3i3m9\hi
copy w3i3m9\hi\mpsdy.dll %mpsreleasename%\lib\w3i3m9\hi
copy w3i3m9\hi\mpsdy.lib %mpsreleasename%\lib\w3i3m9\hi

mkdir %mpsreleasename%\lib\w3i3m9\di
@echo di-%mpsreleasename% > %mpsreleasename%\lib\w3i3m9\di\di-%mpsreleasename%.txt
copy w3i3m9\di\mps.lib %mpsreleasename%\lib\w3i3m9\di
copy w3i3m9\di\mpsplan.lib %mpsreleasename%\lib\w3i3m9\di
copy w3i3m9\di\mpsplcb.lib %mpsreleasename%\lib\w3i3m9\di
copy w3i3m9\di\mpsdy.dll %mpsreleasename%\lib\w3i3m9\di
copy w3i3m9\di\mpsdy.lib %mpsreleasename%\lib\w3i3m9\di

mkdir %mpsreleasename%\lib\w3i3m9\ci
@echo ci-%mpsreleasename% > %mpsreleasename%\lib\w3i3m9\ci\ci-%mpsreleasename%.txt
copy w3i3m9\ci\mps.lib %mpsreleasename%\lib\w3i3m9\ci
copy w3i3m9\ci\mpsplan.lib %mpsreleasename%\lib\w3i3m9\ci
copy w3i3m9\ci\mpsplcb.lib %mpsreleasename%\lib\w3i3m9\ci
copy w3i3m9\ci\mpsdy.dll %mpsreleasename%\lib\w3i3m9\ci
copy w3i3m9\ci\mpsdy.lib %mpsreleasename%\lib\w3i3m9\ci
