# impl.makefile.s7ppac: S7PPAC MPW MAKE FILE
#
# $HopeName: MMsrc!s7ppac:Makefile(trunk.13) $
#
# Copyright (C) 1996.  Harlequin Group plc.  All rights reserved.
#

# This only knows about making the mmsw libraries for MM/EP-core.
# 
# To invoke:
#
#  directory {...}s7ppac
#  BuildProgram -d Variety mmsw
#
# (this builds mmsw_HE.o and mmsw_CI.o)
#
# to clean everything else away:
#
#  directory {...}s7ppac
#  BuildProgram -d Variety clean
#
# Notes:
#
# .dep.accum: If several single-f rules all produce the same product 
#  (left-hand-side of the f), they act together: at most one may have a 
#  body, the others only add additional dependencies.
#
# .he.untested: The variety.he build hasn't been tested; see 
#  .obsolete.zz below.


# Options cribbed from SWbuild!macos:rules:rulemrc4.make(trunk.3)
CC_OPT_CONTROL= -nomfmem -y "{TempFolder}"
CC_OPT_SOURCE= -inclpath nosystem
CC_OPT_OBJECT= -align power -char signed -enum int -fp_contract off -ldsize 128 -target ppc

CC_OPTT_AbsAPAD= -ansi strict -proto strict -typecheck strict
# Plus "warnings are errors":
CC_OPTT={CC_OPTT_AbsAPAD} -w error

CC_OPTR_Dbg= -opt off -sym on -tb on
CC_OPTR_Qkb= -opt off -sym off -tb on
CC_OPTR_Spd= -opt speed,nounroll -sym off -tb on
CC_OPTR_= -opt speed,nounroll -sym off -w 35

SYS_CINCLUDES= -i {CIncludes96}


BB_OPT_CONTROL= -nomf -linkfaster on
BB_OPT_SOURCE=
BB_OPT_OBJECT= -xm Library

BB_OPTR_Dbg= -sym on
BB_OPTR_Qkb= -sym off
BB_OPTR_Spd= -sym off
BB_OPTR_=    -sym off


PRODDEFINES= -d CONFIG_VAR_{Variety} -d CONFIG_PROD_EPCORE

":" Ä "::"

OSffx     = .c.{Variety}.o

.c.CI.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {TargDir}{Default}.c.CI.o ¶
                {CC_OPTT} ¶
                {CC_OPTR_Dbg} ¶
                {CC_OPT_CONTROL} {CC_OPT_SOURCE} {CC_OPT_OBJECT} ¶
                {ENVDEFINES} {PRODDEFINES} ¶
                {SYS_CINCLUDES} ¶
                {DepDir}{Default}.c

.c.HE.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {TargDir}{Default}.c.HE.o ¶
                {CC_OPTT} ¶
                {CC_OPTR_} ¶
                {CC_OPT_CONTROL} {CC_OPT_SOURCE} {CC_OPT_OBJECT} ¶
                {ENVDEFINES} {PRODDEFINES} ¶
                {SYS_CINCLUDES} ¶
                {DepDir}{Default}.c

# version.o has a special rule so that it is always built
version{OSffx} Ä $OutOfDate

mmsw_{Variety}.o  Ä  ¶
		version{OSffx} ¶
		assert{OSffx} ¶
		ring{OSffx} ¶
		mpm{OSffx} ¶
		bt{OSffx} ¶
		arenacl{OSffx} ¶
		arenavmx{OSffx} ¶
		arena{OSffx} ¶
		pool{OSffx} ¶
		poolmfs{OSffx} ¶
		poolmv{OSffx} ¶
		root{OSffx} ¶
		format{OSffx} ¶
		buffer{OSffx} ¶
		lockan{OSffx} ¶
		ref{OSffx} ¶
		trace{OSffx} ¶
		protsw{OSffx} ¶
		shield{OSffx} ¶
		than{OSffx} ¶
		ssan{OSffx} ¶
		mpsi{OSffx} ¶
		ld{OSffx} ¶
		poolepdl{OSffx} ¶
		poolepvm{OSffx} ¶
		poolams{OSffx} ¶
		action{OSffx} ¶
		seg{OSffx} ¶
		event{OSffx} ¶
		mpsioan{OSffx} ¶
		poolmrg{OSffx} ¶
		message{OSffx}

mmsw_CI.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_Dbg} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}

mmsw_HE.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}


## .obsolete.zz: In trunk.11 this makefile was changed radically, but 
##  could not be tested because Mac Release ScriptWorks was broken.  
##  When SW is mended, it might be nice to test and compare the new and 
##  old builds of the MPS.  
##  It is easy to make the old HE build: just use the following "ZZ" 
##  pseudo-variety, which uses the old compiler, linker, and 
##  command-line options to build the HE variety.  
##  To build it: BuildProgram -d Variety=ZZ mmsw_ZZ.o
##  richardk 1998-05-28
.c.ZZ.o  Ä  .c
  {MPWCommands96}MrC1.0 ¶
    -o {TargDir}{Default}.c.ZZ.o ¶
    -d CONFIG_VAR_HE -d CONFIG_PROD_EPCORE ¶
    -ansi strict -proto strict -typecheck strict -w error ¶
    -sym full ¶
    -i "{CIncludes96}" ¶
    {DepDir}{Default}.c
mmsw_ZZ.o  Ä  # .dep.accum
  {MPWCommands96}PPCLink ¶
    -o {Targ} ¶
    -xm l -sym on ¶
    {Deps} "{SharedLibraries96}InterfaceLib"


cleanall Ä
	Delete Å.o

clean Ä
	Delete Å.c.Å.o

mmsw Ä
	BuildProgram -d Variety=HE mmsw_HE.o
	BuildProgram -d Variety=CI mmsw_CI.o
