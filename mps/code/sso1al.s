 # impl.s.sso1al: STACK SCANNING FOR DIGITAL UNIX / ALPHA
 #
 # $HopeName: MMsrc!sso1al.s(trunk.1) $
 # Copyright (C) 1997 Harlequin Group, all rights reserved
 #
 # .readership: Any MPS developer that is prepared to read Alpha
 # assembly code in DIGITAL UNIX 'as' syntax.
 #
 # See design.mps.sso1al for the design (exists).


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
