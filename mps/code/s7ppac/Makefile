# impl.makefile.s7ppac: S7PPAC MPW MAKE FILE
#
# $Id$
#
# Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.

# .limit.epcore: This only knows about making the mmsw libraries for
# MM/EP-core.
# 
# .usage: To invoke:
#
#  directory {...}s7ppac
#  BuildProgram -d Variety mmsw
#
# (this builds mmsw_TI.o, mmsw_HI.o, and mmsw_WE.o)
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

.c.TI.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {Targ} ¶
                {CC_OPTT} ¶
                {CC_OPTR_Dbg} ¶
                {CC_OPT_CONTROL} {CC_OPT_SOURCE} {CC_OPT_OBJECT} ¶
                {ENVDEFINES} {PRODDEFINES} ¶
                {SYS_CINCLUDES} ¶
                {DepDir}{Default}.c

.c.CI.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {Targ} ¶
                {CC_OPTT} ¶
                {CC_OPTR_Dbg} ¶
                {CC_OPT_CONTROL} {CC_OPT_SOURCE} {CC_OPT_OBJECT} ¶
                {ENVDEFINES} {PRODDEFINES} ¶
                {SYS_CINCLUDES} ¶
                {DepDir}{Default}.c

.c.HI.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {Targ} ¶
                {CC_OPTT} ¶
                {CC_OPTR_} ¶
                {CC_OPT_CONTROL} {CC_OPT_SOURCE} {CC_OPT_OBJECT} ¶
                {ENVDEFINES} {PRODDEFINES} ¶
                {SYS_CINCLUDES} ¶
                {DepDir}{Default}.c

.c.HE.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {Targ} ¶
                {CC_OPTT} ¶
                {CC_OPTR_} ¶
                {CC_OPT_CONTROL} {CC_OPT_SOURCE} {CC_OPT_OBJECT} ¶
                {ENVDEFINES} {PRODDEFINES} ¶
                {SYS_CINCLUDES} ¶
                {DepDir}{Default}.c

.c.WE.o  Ä  .c
        {MPWCommands96}MrC2.0.2d1 ¶
                -o {Targ} ¶
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
                boot{OSffx} ¶
                arenacl{OSffx} ¶
                arenavmx{OSffx} ¶
                locus{OSffx} ¶
                arena{OSffx} ¶
                global{OSffx} ¶
                tract{OSffx} ¶
                walk{OSffx} ¶
                reserv{OSffx} ¶
                pool{OSffx} ¶
                poolmfs{OSffx} ¶
                poolmv{OSffx} ¶
                root{OSffx} ¶
                format{OSffx} ¶
                buffer{OSffx} ¶
                ref{OSffx} ¶
                trace{OSffx} ¶
                protsw{OSffx} ¶
                shield{OSffx} ¶
                than{OSffx} ¶
                ssan{OSffx} ¶
                mpsi{OSffx} ¶
                ld{OSffx} ¶
		poolabs{OSffx} ¶
                poolepdl{OSffx} ¶
                poolepvm{OSffx} ¶
                poolams{OSffx} ¶
		protocol{OSffx} ¶
                action{OSffx} ¶
                seg{OSffx} ¶
                sac{OSffx} ¶
                event{OSffx} ¶
                mpsioan{OSffx} ¶
                dbgpool{OSffx} ¶
		meter{OSffx} ¶
                poolmrg{OSffx} ¶
                poolmvff{OSffx} ¶
                splay{OSffx} ¶
                cbs{OSffx} ¶
                message{OSffx}

mmsw_TI.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_Dbg} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}

mmsw_CI.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_Dbg} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}

mmsw_HI.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}

mmsw_HE.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}

mmsw_WE.o  Ä  # .dep.accum
        {MPWCommands97}PPCLink1.5 ¶
                -o {Targ} ¶
                {BB_OPTR_} ¶
                {BB_OPT_CONTROL} {BB_OPT_SOURCE} {BB_OPT_OBJECT} ¶
                {Deps}


cleanall Ä
        Delete Å.o

clean Ä
        Delete Å.c.Å.o

mmsw Ä
        BuildProgram -d Variety=TI mmsw_TI.o
        BuildProgram -d Variety=HI mmsw_HI.o
        BuildProgram -d Variety=WE mmsw_WE.o


# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
# All rights reserved.  This is an open source license.  Contact
# Ravenbrook for commercial licensing options.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 
# 3. Redistributions in any form must be accompanied by information on how
# to obtain complete source code for this software and any accompanying
# software that uses this software.  The source code must either be
# included in the distribution or be available for no more than the cost
# of distribution plus a nominal fee, and must be freely redistributable
# under reasonable conditions.  For an executable file, complete source
# code means the source code for all modules it contains. It does not
# include source code for modules or files that typically accompany the
# major components of the operating system on which the executable file
# runs.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
# USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
