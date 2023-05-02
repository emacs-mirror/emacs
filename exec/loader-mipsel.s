# Copyright (C) 2023 Free Software Foundation, Inc.
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

# Make sure not to use t4 through t7, in order to maintain portability
# with N32 ABI systems.

	.set noreorder			# delay slots managed by hand
	.section .text
	.global __start
__start:
	li	$v0, SYSCALL_nanosleep	# SYS_nanosleep
	la	$a0, .timespec		# rqtp
	li	$a1, 0			# rmtp
	syscall				# syscall
	lw	$s6, ($sp)		# original stack pointer
	addi	$s0, $sp, 8		# start of load area
	addi	$sp, -8			# primary fd, secondary fd
	li	$t0, -1			# secondary fd
	sw	$t0, 4($sp)		# initialize secondary fd
.next_action:
	lw	$s2, ($s0)		# action number
	nop				# delay slot
	andi	$t0, $s2, 15		# t0 = s2 & 15
	beqz	$t0, .open_file		# open file?
	li	$t1, 3			# t1 = 3, delay slot
	beq	$t0, $t1, .rest_of_exec	# jump to code
	li	$t1, 4			# t1 = 4, delay slot
	beq	$t0, $t1, .do_mmap_anon	# anonymous mmap
.do_mmap:
	lw	$a0, 4($s0)		# vm_address, delay slot
	lw	$v1, 8($s0)		# file_offset
	lw	$a2, 12($s0)		# protection
	lw	$a1, 16($s0)		# length
	lw	$a3, 20($s0)		# flags
	lw	$v0, ($sp)		# primary fd
	andi	$t1, $s2, 16		# t1 = s2 & 16
	beqz	$t1, .do_mmap_1		# secondary fd?
	nop				# delay slot
	lw	$v0, 4($sp)		# secondary fd
	nop				# delay slot
.do_mmap_1:
SYSCALL(`$v0',`$v1',`$zero',`$zero')	# syscall args
	li	$v0, SYSCALL_mmap	# SYS_mmap
	syscall				# syscall
	bne	$a3, $zero, .perror	# perror
RESTORE()				# delay slot, restore sp
	lw	$s5, 24($s0)		# clear
	add	$t0, $a0, $a1		# t0 = length + vm_address, delay slot
	sub	$t1, $t0, $s5		# t1 = t0 - clear
.align:
	beq	$t0, $t1, .continue	# already finished?
	nop				# delay slot
	andi	$t2, $t1, 3		# t1 & 3?
	bnez	$t2, .fillw		# start filling longs
	nop				# delay slot
	sb	$zero, ($t1)		# clear byte
	addi	$t1, $t1, 1		# t1++
	j	.align			# continue
	nop				# delay slot
.fillw:
	sub	$t2, $t0, $t1		# t2 = t0 - t1
	sltiu	$t2, $t2, 32		# r2 < 32?
	bne	$t2, $zero, .fillb	# fill bytes
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
	j	.fillw			# fill either word or byte
	nop				# delay slot
.fillb:
	beq	$t0, $t1, .continue	# already finished?
	nop				# delay slot
	sb	$zero, ($t1)		# clear byte
	addi	$t1, $t1, 1		# t1++
.continue:
	addi	$s0, $s0, 28		# s0 = next action
	j	.next_action		# next action
	nop				# delay slot
.do_mmap_anon:
	lw	$v1, 8($s0)		# file_offset
	lw	$a2, 12($s0)		# protection
	lw	$a1, 16($s0)		# length
	lw	$a3, 20($s0)		# flags
	li	$t4, -1			# fd
	j	.do_mmap_1		# do mmap
	nop				# delay slot
.open_file:
	li	$v0, SYSCALL_open	# SYS_open
	addi	$a0, $s0, 4		# start of name
	move	$a1, $zero		# flags = O_RDONLY
	move	$a2, $zero		# mode = 0
	syscall				# syscall
	bne	$a3, $zero, .perror	# perror
	addi	$s0, $s0, 4		# start of string, delay slot
	move	$t3, $s0		# t3 = char past separator
.nextc:
	lb	$t0, ($s0)		# load byte
	addi	$s0, $s0, 1		# s0++
	li	$t1, 47			# directory separator `/'
	bne	$t0, $t1, .nextc1	# is separator char?
	nop				# delay slot
	move	$t3, $s0		# t3 = char past separator
.nextc1:
	bnez	$t0, .nextc		# next character?
	nop				# delay slot
	addi	$s0, $s0, 3		# adjust for round
	li	$t2, -4			# t2 = -4
	and	$s0, $s0, $t2		# mask for round
	andi	$t0, $s2, 16		# t1 = s2 & 16
	beqz	$t0, .primary		# primary fd?
	move	$t0, $sp		# address of primary fd, delay slot
	addi	$t0, $t0, 4		# address of secondary fd
	j	.next_action		# next action
.primary:
	sw	$v0, ($t0)		# store fd, delay slot
	li	$v0, SYSCALL_prctl	# SYS_prctl
	li	$a0, 15			# PR_SET_NAME
	move	$a1, $t3		# name
	move	$a2, $zero		# arg1
	move	$a3, $zero		# arg2
SYSCALL(`$a2',`$a2',`$a2',`$a2')	# syscall args
	syscall				# syscall
RESTORE()				# restore sp
	j	.next_action		# next action
	nop				# delay slot
.perror:
	move	$a0, $v0		# errno
	li	$v0, SYSCALL_exit	# SYS_exit
	syscall				# syscall
.rest_of_exec:
	move	$s1, $s6		# s1 = original SP
	lw	$t0, ($s1)		# argc
	nop				# delay slot
	sll	$t0, $t0, 2		# argc *= 4
	addi	$t0, $t0, 8		# argc += 8
	add	$s1, $s1, $t0		# s1 = start of envp
.skipenv:
	lw	$t0, ($s1)		# t0 = *s1
	addi	$s1, $s1, 4		# s1++
	bne	$t0, $zero, .skipenv	# skip again
	nop				# delay slot
	la	$s2, .auxvtab		# address of auxv table
.one_auxv:
	lw	$t0, ($s1)		# t0 = auxv type
	li	$t1, 10			# t1 = 10, delay slot
	beqz	$t0, .finish		# is AT_IGNORE?
	sltu	$t1, $t0, $t1		# t1 = t0 < num offsets, delay slot
	beq	$t1, $zero, .next	# next auxv
	sll	$t1, $t0, 2		# t1 = t0 * 4, delay slot
	add	$t1, $s2, $t1		# t1 = .auxvtab + t1
	lw	$t2, ($t1)		# t2 = *t1
	nop				# delay slot
	beqz	$t2, .next		# skip auxv
	add	$t2, $s0, $t2		# t2 = s0 + t2
	lw	$t2, ($t2)		# t2 = *t2
	nop				# delay slot
	sw	$t2, 4($s1)		# set auxv value
.next:
	addi	$s1, $s1, 8		# next auxv
	j	.one_auxv		# next auxv
	nop				# delay slot
.finish:
	lw	$t0, 4($sp)		# secondary fd
	lw	$s1, ($sp)		# primary fd, delay slot, preserved
	li	$t2, -1			# immediate -1
	beq	$t0, $t2, .finish1	# secondary fd set?
	li	$v0, SYSCALL_close	# SYS_close, delay slot
	move	$a0, $t0		# fd
	syscall				# syscall
	li	$v0, SYSCALL_close	# SYS_close
.finish1:
	move	$a0, $s1		# primary fd
	syscall				# syscall
	li	$v0, SYSCALL_prctl	# SYS_prctl
	li	$a0, 45			# PR_SET_FP_MODE
	lw	$a1, 28($s0)		# fpu_mode
	move	$a2, $zero		# arg3
	move	$a3, $zero		# arg4
SYSCALL(`$a2',`$a2',`$a2',`$a2')	# syscall args
	syscall				# syscall
RESTORE()				# restore sp
.jump:
	move	$v0, $zero		# rtld_fini
	lw	$t0, 4($s0)		# entry
	move	$sp, $s6		# restore stack pointer, delay slot
	jr	$t0			# enter
	nop				# delay slot

.auxvtab:
	.long	0			# 0
	.long	0			# 1
	.long	0			# 2
	.long	20			# 3 AT_PHDR
	.long	12			# 4 AT_PHENT
	.long	16			# 5 AT_PHNUM
	.long	0			# 6
	.long	24			# 7 AT_BASE
	.long	0			# 8
	.long	8			# 9 AT_ENTRY

.timespec:
	.long	10
	.long	10

# Local Variables:
# asm-comment-char: 35
# End:
