# Copyright (C) 2023-2026 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# GNU Emacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.	 If not, see <https://www.gnu.org/licenses/>.

include(`config-mips.m4')

## Beware: $t0-$t4 alias the syscall (and function, but they are not
## material in this context) argument registers on N32 systems, and
## mustn't be relied upon to hold arguments to `SYSCALL'.

	.set noreorder			# delay slots managed by hand
	.section .text
	.global __start
__start:
	## li	$v0, SYSCALL_nanosleep	# SYS_nanosleep
	## la	$a0, timespec		# rqtp
	## li	$a1, 0			# rmtp
	## syscall			# syscall
	lw	$s6, ($sp)		# original stack pointer
	addi	$s0, $sp, 8		# start of load area
	addi	$sp, -8			# primary fd, secondary fd
	li	$t0, -1			# secondary fd
	sw	$t0, 4($sp)		# initialize secondary fd
next_action:
	lw	$s2, ($s0)		# action number
	andi	$t0, $s2, 15		# t0 = s2 & 15
	beqz	$t0, open_file		# open file?
	li	$t1, 3			# t1 = 3, delay slot
	beq	$t0, $t1, rest_of_exec	# jump to code
	li	$t1, 4			# t1 = 4, delay slot
	beq	$t0, $t1, do_mmap_anon	# anonymous mmap
do_mmap:
	lw	$a0, 4($s0)		# vm_address, delay slot
	lw	$v1, 8($s0)		# file_offset
	lw	$a2, 12($s0)		# protection
	lw	$a1, 16($s0)		# length
	lw	$a3, 20($s0)		# flags
	lw	$v0, ($sp)		# primary fd
	andi	$t1, $s2, 16		# t1 = s2 & 16
	beqz	$t1, do_mmap_1		# secondary fd?
	nop				# delay slot
	lw	$v0, 4($sp)		# secondary fd
	nop				# delay slot
do_mmap_1:
SYSCALL(`$v0',`$v1',`$zero',`$zero')	# syscall args
	li	$v0, SYSCALL_mmap	# SYS_mmap
	syscall				# syscall
	bnez	$a3, perror		# perror
RESTORE()				# delay slot, restore sp
	lw	$s5, 24($s0)		# clear
	add	$t0, $a0, $a1		# t0 = length + vm_address, delay slot
	sub	$t1, $t0, $s5		# t1 = t0 - clear
align:
	beq	$t0, $t1, continue	# already finished?
	nop				# delay slot
	andi	$t2, $t1, 3		# t1 & 3?
	bnez	$t2, fillw		# start filling longs
	nop				# delay slot
	sb	$zero, ($t1)		# clear byte
	addi	$t1, $t1, 1		# t1++
	j	align			# continue
	nop				# delay slot
fillw:
	sub	$t2, $t0, $t1		# t2 = t0 - t1
	sltiu	$t2, $t2, 32		# r2 < 32?
	bne	$t2, $zero, fillb	# fill bytes
	nop				# delay slot
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	sw	$zero, ($t1)		# zero word
	addi	$t1, $t1, 4		# next word
	j	fillw			# fill either word or byte
	nop				# delay slot
fillb:
	beq	$t0, $t1, continue	# already finished?
	nop				# delay slot
	sb	$zero, ($t1)		# clear byte
	addi	$t1, $t1, 1		# t1++
continue:
	addi	$s0, $s0, 28		# s0 = next action
	j	next_action		# next action
	nop				# delay slot
do_mmap_anon:
	lw	$v1, 8($s0)		# file_offset
	lw	$a2, 12($s0)		# protection
	lw	$a1, 16($s0)		# length
	lw	$a3, 20($s0)		# flags
	j	do_mmap_1		# do mmap
	li	$v0, -1			# fd, delay slot
open_file:
	li	$v0, SYSCALL_open	# SYS_open
	addi	$a0, $s0, 4		# start of name
	move	$a1, $zero		# flags = O_RDONLY
	move	$a2, $zero		# mode = 0
	syscall				# syscall
	bne	$a3, $zero, perror	# perror
	addi	$s0, $s0, 4		# start of string, delay slot
	move	$t3, $s0		# t3 = char past separator
nextc:
	lb	$t0, ($s0)		# load byte
	addi	$s0, $s0, 1		# s0++
	li	$t1, 47			# directory separator `/'
	bne	$t0, $t1, nextc1	# is separator char?
	nop				# delay slot
	move	$t3, $s0		# t3 = char past separator
nextc1:
	bnez	$t0, nextc		# next character?
	nop				# delay slot
	addi	$s0, $s0, 3		# adjust for round
	li	$t2, -4			# t2 = -4
	and	$s0, $s0, $t2		# mask for round
	andi	$t0, $s2, 16		# t1 = s2 & 16
	beqz	$t0, primary		# primary fd?
	move	$t0, $sp		# address of primary fd, delay slot
	addi	$t0, $t0, 4		# address of secondary fd
	j	next_action		# next action
primary:
	sw	$v0, ($t0)		# store fd, delay slot
	li	$v0, SYSCALL_prctl	# SYS_prctl
	li	$a0, 15			# PR_SET_NAME
	move	$a1, $t3		# name
	move	$a2, $zero		# arg1
	move	$a3, $zero		# arg2
SYSCALL(`$a2',`$a2',`$a2',`$a2')	# syscall args
	syscall				# syscall
RESTORE()				# restore sp
	j	next_action		# next action
	nop				# delay slot
perror:
	move	$a0, $v0		# errno
	li	$v0, SYSCALL_exit	# SYS_exit
	syscall				# syscall
rest_of_exec:
	move	$s1, $s6		# s1 = original SP
	lw	$t0, ($s1)		# argc
	sll	$t0, $t0, 2		# argc *= 4
	addi	$t0, $t0, 8		# argc += 8
	add	$s1, $s1, $t0		# s1 = start of envp
skip_environ:
	/* Locate the auxiliary vector.  */
1:	lw	$t0, ($s1)		# t0 = *s1
	bnez	$t0, 1b			# skip environment entry
	addi	$s1, $s1, 4		# s1++
	move	$s2, $s1		# $s2 = end of environment
1:	lw	$t0, ($s1)		# t0 = *s1
	bnez	$t0, 1b			# skip auxiliary vector entry
	addi	$s1, $s1, 8		# (Elf32_auxv_t *) s1++
	/* Decide how many bytes must be copied and where to
	   save the file name.  Move the stack pointer to a safe
	   position below any data that must be preserved.  */
	lw	$t1, 32($s0)		# length of string
	addi	$t1, $t1, 1
	addi	$t2, $s0, 36		# pointer to string
	sub	$t3, $s1, $s6		# number of bytes in vectors
	sub	$t0, $s1, $t1		# position of string
	and	$t0, $t0, -16		# align value
	sub	$t3, $t0, $t3		# position of argc
	and	$t3, $t3, -16		# align value
	/* Move the stack pointer and save required information.
	   4(FP)   = secondary/interpreter fd.
	   0(FP)   = primary/executable fd.
	   -4(FP)  = cmd->entry
	   -8(FP)  = cmd->at_entry
	   -12(FP) = cmd->at_phent
	   -16(FP) = cmd->at_phnum
	   -20(FP) = cmd->at_phdr
	   -24(FP) = cmd->at_base
	   -28(FP) = cmd->fpu_mode (only significant when N32)
	   $sp = copy of string.  */
	move	$t4, $sp		# current sp
	sub	$t5, $t3, $sp		# new argc - current sp
	blt	$t5, 8, 1f		# more than two slots apart
	addi	$sp, $t3, -8		# $sp = two slots below new argc
	j	2f			# skip copying fds
	move	$sp, $t4		# retain current sp
1:	lw	$t5, ($t4)		# old primary fd
	sw	$t5, ($sp)		# save the same
	lw	$t5, 4($t4)		# old interpreter fd
	sw	$t5, 4($sp)		# save the same
2:	move	FP, $sp			# set base pointer
	addi	$sp, $sp, -28		# command data
	lw	$t5, 4($s0)		# entry
	lw	$t6, 8($s0)		# at_entry
	sw	$t5, -4(FP)		# save entry
	sw	$t6, -8(FP)		# save at_entry
	lw	$t5, 12($s0)		# at_phent
	lw	$t6, 16($s0)		# at_phnum
	sw	$t5, -12(FP)		# save at_phent
	sw	$t6, -16(FP)		# save at_phnum
	lw	$t5, 20($s0)		# at_phdr
	lw	$t6, 24($s0)		# at_base
	sw	$t5, -20(FP)		# save at_phdr
	sw	$t6, -24(FP)		# save at_base
	lw	$t5, 28($s0)		# fpu_mode
	sw	$t5, -28(FP)		# save fpu_mode
	sub	$sp, $sp, $t1		# space for string
	/* Save the input string.  */
	add	$t5, $t2, $t1		# end of source ($t2)
	move	$t6, $sp		# dst
	move	$s0, $t1		# $s0 = length of string
	/* src = $t2, dst = $t6 */
	bgeu	$t2, $t5, 2f		# there already?
	nop
1:	lb	$t1, ($t2)		# $t1 = *$t2
	addi	$t2, $t2, 1		# $t2++
	addi	$t6, $t6, 1		# $t6++
	bltu	$t2, $t5, 1b
	sb	$t1, -1($t6)		# *($t6 - 1) = $t1
2:	move	$s3, $sp		# copy of string
	and	$sp, $sp, -16		# align stack
copy_env_and_args:
	/* Copy argc, argv, and the environment array.
	   $t4 = destination, $t5 = src, $s2 = src_end  */
	move	$t4, $t3		# destination of argc
	move	$t5, $s6		# original SP
	bgeu	$t5, $s2, 2f		# there already?
	nop
1:	lw	$t1, ($t5)		# $t1 = *src
	addi	$t5, $t5, 4		# src++
	addi	$t4, $t4, 4		# dst++
	bltu	$t5, $s2, 1b		# src < src_end
	sw	$t1, -4($t4)		# *(dst - 4) = $t1
copy_auxv:
	/* $t4 = destination, $t5 = first auxval.  */
2:	lw	$t1, ($t5)		# a_type
	lw	$t2, 4($t5)		# a_un.a_val
	addi	$t4, $t4, 8		# (Elf32_auxv_t *) dst++
	addi	$t5, $t5, 8		# (Elf32_auxv_t *) src++
	beqz	$t1, 8f			# AT_NULL
	li	$t6, 3
	beq	$t1, $t6, 1f		# AT_PHDR
	li	$t6, 4
	beq	$t1, $t6, 2f		# AT_PHENT
	li	$t6, 5
	beq	$t1, $t6, 3f		# AT_PHNUM
	li	$t6, 9
	beq	$t1, $t6, 4f		# AT_ENTRY
	li	$t6, 7
	beq	$t1, $t6, 5f		# AT_BASE
	li	$t6, 31
	beq	$t1, $t6, 6f		# AT_EXECFN
	nop
	b	7f
	nop
1:	b	7f
	lw	$t2, -20(FP)
2:	b	7f
	lw	$t2, -12(FP)
3:	b	7f
	lw	$t2, -16(FP)
4:	b	7f
	lw	$t2, -8(FP)
5:	b	7f
	lw	$t2, -24(FP)
6:	b	7f
	move	$t2, $t0
7:	sw	$t1, -8($t4)		# dst->a_type
	j	copy_auxv
	sw	$t2, -4($t4)		# dst->a_un.a_val
	/* Copy the final element.  */
8:	sw	$t1, -8($t4)		# dst->a_type
	sw	$t2, -4($t4)		# dst->a_un.a_val
finish:
	/* Copy the string to its position in auxv
	   (src = $s3, dst = $t0).  */
	add	$t1, $s3, $s0		# src end
	bgeu	$s3, $t1, 2f		# there already?
	nop
1:	lb	$t2, ($s3)		# c = *src
	addi	$s3, $s3, 1		# *src++
	addi	$t0, $t0, 1		# dst++
	bltu	$s3, $t1, 1b
	sb	$t2, -1($t0)	   	# *(dst - 1) = c
	/* Save variables.  */
2:	move	$s6, $t3	   	# new stack pointer
	lw	$t4, 4(FP)		# secondary fd
	lw	$s1, (FP)		# primary fd, delay slot, preserved
	li	$t2, -1			# immediate -1
	beq	$t4, $t2, finish1	# secondary fd set?
	li	$v0, SYSCALL_close	# SYS_close, delay slot
	move	$a0, $t4		# fd
	syscall				# syscall
	li	$v0, SYSCALL_close	# SYS_close
finish1:
	move	$a0, $s1		# primary fd
	syscall				# syscall
	li	$v0, SYSCALL_prctl	# SYS_prctl
	li	$a0, 45			# PR_SET_FP_MODE
	lw	$a1, -28(FP)		# fpu_mode
	move	$a2, $zero		# arg3
	move	$a3, $zero		# arg4
SYSCALL(`$a2',`$a2',`$a2',`$a2')	# syscall args
	syscall				# syscall
RESTORE()				# restore sp
jump:
	move	$v0, $zero		# rtld_fini
	lw	$t9, -4(FP)		# entry
	move	$sp, $s6		# restore stack pointer, delay slot
        /* Clear at least one page's worth of stack.  glibc on mipsel
	   copies certain fields from the stack to the `link_map'
	   structure representing ld.so, which are not subsequently
	   replaced if otherwise than zero.

	   XXX: report this glibc bug?  */
	addi	$v0, $sp, -4096
	and	$v0, $v0, -4095
1:	sw	$zero, ($v0)		# copy 32 byte blocks
	sw	$zero, 4($v0)
	sw	$zero, 8($v0)
	sw	$zero, 12($v0)
	sw	$zero, 16($v0)
	sw	$zero, 20($v0)
	sw	$zero, 24($v0)
	sw	$zero, 28($v0)
	addi	$v0, $v0, 32
	sub	$t0, $sp, $v0		# remainder
	bge	$t0, 32, 1b		# test remainder
	nop				# copy 4 byte blocks
	beqz	$t0, 2f
1:	addi	$v0, $v0, 4
	bltu	$v0, $sp, 1b
	sw	$zero, -4($v0)
2:	jr	$t9			# enter
	nop				# delay slot

## timespec:
## 	.long	10
## 	.long	10

# Local Variables:
# asm-comment-char: ?#
# End:
