@rem impl.bat.gathconf: GATHERING A RELEASE FOR CONFIGURA
@rem
@rem $HopeName: !gathconf.bat(trunk.1) $
@rem Copyright (C) 2000 Harlequin Limited.  All rights reserved.

rmdir /s w3i3mv
nmake /f w3i3mv.nmk VARIETY=we mps_conf.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=wi mps_conf.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=ce mps_conf.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=ci mps_conf.lib mpsplan.lib
mkdir release
mkdir release\include
mkdir release\lib
mkdir release\lib\w3i3
mkdir release\lib\w3i3\we
mkdir release\lib\w3i3\ce
mkdir release\lib\w3i3\wi
mkdir release\lib\w3i3\ci
mkdir release\src
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
copy mpstd.h release\include
copy mpsw3.h release\include
copy mpswin.h release\include
copy w3i3mv\we\mps_conf.lib release\lib\w3i3\we
copy w3i3mv\ce\mps_conf.lib release\lib\w3i3\ce
copy w3i3mv\we\mpsplan.lib release\lib\w3i3\we
copy w3i3mv\ce\mpsplan.lib release\lib\w3i3\ce
copy w3i3mv\wi\mps_conf.lib release\lib\w3i3\wi
copy w3i3mv\ci\mps_conf.lib release\lib\w3i3\ci
copy w3i3mv\wi\mpsplan.lib release\lib\w3i3\wi
copy w3i3mv\ci\mpsplan.lib release\lib\w3i3\ci
copy mpsliban.c release\src
copy mpsioan.c release\src
