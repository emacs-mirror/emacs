# ==== S7PPAC MPW MAKE FILE ====
#
# $HopeName: MMsrc!s7ppac:Makefile(trunk.3) $
#
# Copyright (C) 1996 Harlequin Group, all rights reserved
#

# This only knows about making the mmsw libraries for MM/EP-core.
# 
# To invoke:
#
#  BuildProgram -d Variety mmsw
#
# (this builds mmsw_RO.o and mmsw_DP.o)
#
# to clean everything else away:
#
#  BuildProgram -d Variety clean

COptions    = -ansi strict -proto strict -typecheck strict -w error ¶
              -d TARGET_VAR_{Variety} -d TARGET_ARENA_CLIENT -sym full

":" Ä "::"

OSffx     = .c.{Variety}.o

{OSffx} 	Ä	.c
	MrC {DepDir}{Default}.c -o {TargDir}{Default}{OSffx} {COptions}

mmsw_{Variety}.o Ä assert{OSffx} ring{OSffx} mpm{OSffx} ¶
                   arenacl{OSffx} space{OSffx} pool{OSffx} poolmfs{OSffx} ¶
                   poolmv{OSffx} root{OSffx} format{OSffx} buffer{OSffx} ¶
                   lockan{OSffx} ref{OSffx} trace{OSffx} than{OSffx} ¶
                   protsw{OSffx} shield{OSffx} mpsisw{OSffx} ld{OSffx} ¶
                   ssan{OSffx} poolepdl{OSffx} action{OSffx} seg{OSffx}
	PPCLink {Deps} -xm l -o {Targ} -sym on

cleanall Ä
	Delete Å.o

clean Ä
	Delete Å.c.Å.o
	
mmsw Ä
	BuildProgram -d Variety=RO mmsw_RO.o
	BuildProgram -d Variety=DP mmsw_DP.o
	