 # sso1al.s: STACK SCANNING FOR DIGITAL UNIX / ALPHA
 #
 # $Id$
 # Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 #
 # .readership: Any MPS developer that is prepared to read Alpha
 # assembly code in DIGITAL UNIX 'as' syntax.
 #
 # See <design/sso1al/> for the design (exists).


.globl	StackScan
.globl	TraceScanArea

.ent	StackScan
StackScan:
ldgp	$gp,0($27)	# faff with global pointer
lda	$sp,-64($sp)	# build and declare frame and saveregs
.frame	$sp,64,$26
.mask	0x400FE00,-64
stq	$26,0($sp)	# dump ra and other regs so that they get fixed
stq	$9,8($sp)
stq	$10,16($sp)
stq	$11,24($sp)
stq	$12,32($sp)
stq	$13,40($sp)
stq	$14,48($sp)
stq	$15,56($sp)
.prologue	1

 # bis $31,$16,$16 1st arg to TraceScanArea is same as our 1st arg
bis	$31,$17,$18	# area to be scanned is from $sp to StackBot
bis	$31,$sp,$17

jsr	$26,TraceScanArea
ldgp	$gp,0($26)
 # our result is TraceScanArea's result, so leave $0 untouched

ldq	$26,0($sp)
lda	$sp,+64($sp)
ret	$31,($26),1
.end	StackScan

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
