# impl.makefile.s7ppac: S7PPAC MPW MAKE FILE
#
# $HopeName: MMsrc!s7ppac:Makefile(trunk.7) $
#
# Copyright (C) 1996, 1997 Harlequin Group, all rights reserved
#

# This only knows about making the mmsw libraries for MM/EP-core.
# 
# To invoke:
#
#  BuildProgram -d Variety mmsw
#
# (this builds mmsw_HE.o and mmsw_CI.o)
#
# to clean everything else away:
#
#  BuildProgram -d Variety clean

COptions    = -ansi strict -proto strict -typecheck strict -w error ¶
              -d CONFIG_VAR_{Variety} -d CONFIG_PROD_EPCORE -sym full -i "{CIncludes96}"

":" Ä "::"

OSffx     = .c.{Variety}.o

{OSffx} 	Ä	.c
	{MPWCommands96}MrC1.0 {DepDir}{Default}.c -o {TargDir}{Default}{OSffx} {COptions}

mmsw_{Variety}.o Ä assert{OSffx} ring{OSffx} mpm{OSffx} bt{OSffx} ¶
                   arenacl{OSffx} arena{OSffx} pool{OSffx} poolmfs{OSffx} ¶
                   poolmv{OSffx} root{OSffx} format{OSffx} buffer{OSffx} ¶
                   lockan{OSffx} ref{OSffx} trace{OSffx} protsw{OSffx} ¶
                   shield{OSffx} than{OSffx} ssan{OSffx} mpsi{OSffx} ld{OSffx} ¶
                   poolepdl{OSffx} poolepvm{OSffx} action{OSffx} seg{OSffx} ¶
                   poolmrg{OSffx} message{OSffx}
	{MPWCommands96}PPCLink {Deps} -xm l -o {Targ} -sym on "{SharedLibraries96}InterfaceLib"

cleanall Ä
	Delete Å.o

clean Ä
	Delete Å.c.Å.o

mmsw Ä
	BuildProgram -d Variety=HE mmsw_HE.o
	BuildProgram -d Variety=CI mmsw_CI.o
