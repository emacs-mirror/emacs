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

	.set noreorder			# delay slots managed by hand
	.set noat			# no assembler macros
	.section .text
	.global __start
__start:
dnl	li	$v0, 5034		# SYS_nanosleep
dnl	dla	$a0, .timespec		# rqtp
dnl	li	$a1, 0			# rmtp
dnl	syscall				# syscall
	ld	$s2, ($sp)		# original stack pointer
	DADDI3(	$s0, $sp, 16)		# start of load area
	DADDI2(	$sp, -16)		# primary fd, secondary fd
	li	$t0, -1			# secondary fd
	sd	$t0, 8($sp)		# initialize secondary fd
.next_action:
	ld	$s1, ($s0)		# action number
	andi	$t0, $s1, 15		# t0 = action number & 15
	beqz	$t0, .open_file		# open file?
	nop				# delay slot
	DADDI2(	$t0, -3)		# t0 -= 3
	beqz	$t0, .rest_of_exec	# jump to code
	nop				# delay slot
	li	$t1, 1
	beq	$t0, $t1, .do_mmap_anon	# anonymous mmap?
	nop				# delay slot
.do_mmap:
	ld	$t0, 8($s0)		# vm address
	ld	$t1, 16($s0)		# file_offset
	ld	$t2, 24($s0)		# protection
	ld	$t3, 32($s0)		# length
	ld	$v0, 40($s0)		# flags
	ld	$v1, ($sp)		# primary fd
	andi	$s3, $s1, 16		# s1 & 16?
	beqz	$s3, .do_mmap_1		# secondary fd?
	nop				# delay slot
	ld	$v1, 8($sp)		# secondary fd
.do_mmap_1:
	move	$a0, $t0		# syscall arg
	move	$a1, $t3		# syscall arg
	move	$a2, $t2		# syscall arg
	move	$a3, $v0		# syscall arg
	move	$a4, $v1		# syscall arg
	move	$a5, $t1		# syscall arg
	li	$v0, 5009		# SYS_mmap
	syscall				# syscall
	bne	$a3, $zero, .perror	# perror?
	nop				# delay slot
	ld	$t1, 48($s0)		# clear
	dadd	$t0, $a0, $a1		# t0 = end of mapping
	dsub	$t1, $t0, $t1		# t1 = t0 - clear
.align:
	beq	$t0, $t1, .continue	# already finished
	nop				# delay slot
	andi	$t2, $t1, 7		# t1 & 7?
	bnez	$t2, .filld		# start filling longs
	nop				# delay slot
.filld:
	dsub	$t2, $t0, $t1		# t2 = t0 - t1
	sltiu	$t2, $t2, 64		# t2 < 64?
	bne	$t2, $zero, .fillb	# fill bytes
	nop				# delay slot
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	sd	$zero, ($t1)		# zero doubleword
	DADDI2(	$t1, 8)			# next doubleword
	j	.filld			# fill either doubleword or byte
	nop				# delay slot
.fillb:
	beq	$t0, $t1, .continue	# already finished?
	nop				# delay slot
	sb	$zero, ($t1)		# clear byte
	DADDI2(	$t1, 1)			# t1++
.continue:
	DADDI2(	$s0, 56)		# s0 = next action
	j	.next_action		# next action
	nop				# delay slot
.do_mmap_anon:
	ld	$t0, 8($s0)		# vm address
	ld	$t1, 16($s0)		# file_offset
	ld	$t2, 24($s0)		# protection
	ld	$t3, 32($s0)		# length
	ld	$v0, 40($s0)		# flags
	li	$v1, -1			# fd
	j	.do_mmap_1		# do mmap
	nop				# branch delay slot
.open_file:
	li	$v0, 5002		# SYS_open
	DADDI3(	$a0, $s0, 8)		# start of name
	move	$a1, $zero		# flags = O_RDONLY
	move	$a2, $zero		# mode = 0
	syscall				# syscall
	bne	$a3, $zero, .perror	# perror
	nop				# delay slot
	DADDI2(	$s0, 8)			# start of string
	move	$t3, $s0		# t3 = s0
.nextc:
	lb	$t0, ($s0)		# load byte
	DADDI2(	$s0, 1)			# s0++
	li	$t1, 47			# directory separator `/'
	bne	$t0, $t1, .nextc1	# is separator char?
	nop				# delay slot
	move	$t3, $s0		# t3 = char past separator
.nextc1:
	bnez	$t0, .nextc		# next character?
	nop				# delay slot
	DADDI2(	$s0, 7)			# adjust for round
	li	$t2, -8			# t2 = -8
	and	$s0, $s0, $t2		# mask for round
	andi	$t0, $s1, 16		# t1 = s1 & 16
	move	$t1, $sp		# address of primary fd
	beqz	$t0, .primary		# primary fd?
	nop				# delay slot
	DADDI2(	$t1, 8)			# address of secondary fd
	sd	$v0, ($t1)		# store fd
	j	.next_action		# next action
	nop				# delay slot
.primary:
	sd	$v0, ($t1)		# store fd
	li	$v0, 5153		# SYS_prctl
	li	$a0, 15			# PR_SET_NAME
	move	$a1, $t3		# char past separator
	move	$a2, $zero		# a2
	move	$a3, $zero		# a3
	move	$a4, $zero		# a4
	move	$a5, $zero		# a5
	syscall				# syscall
	j	.next_action		# next action
	nop				# delay slot
.perror:
	move	$a0, $v0		# errno
	li	$v0, 5058		# SYS_exit
	syscall				# syscall
.rest_of_exec:
	move	$s1, $s2		# original SP
	ld	$t0, ($s1)		# argc
	dsll	$t0, $t0, 3		# argc *= 3
	DADDI2(	$t0, 16)		# argc += 16
	dadd	$s1, $s1, $t0		# s1 = start of envp
.skipenv:
	ld	$t0, ($s1)		# t0 = *s1
	DADDI2(	$s1, 8)			# s1++
	bne	$t0, $zero, .skipenv	# skip again
	nop				# delay slot
	dla	$t3, .auxvtab		# address of auxv table
.one_auxv:
	ld	$t0, ($s1)		# t0 = auxv type
	li	$t1, 10			# t1 = 10
	beqz	$t0, .finish		# is AT_IGNORE?
	nop				# delay slot
	sltu	$t1, $t0, $t1		# t1 = t0 < num offsets
	beqz	$t1, .next		# next auxv
	nop				# delay slot
	dsll	$t1, $t0, 2		# t1 = t0 * 4
	dadd	$t1, $t3, $t1		# t1 = .auxvtab + t1
	lw	$t2, ($t1)		# t2 = *t1
	beqz	$t2, .next		# skip auxv
	nop				# delay slot
	dadd	$t2, $s0, $t2		# t2 = s0 + t2
	ld	$t2, ($t2)		# t2 = *t2
	sd	$t2, 8($s1)		# set auxv value
.next:
	DADDI2(	$s1, 16)		# next auxv
	j	.one_auxv		# next auxv
	nop				# delay slot
.finish:
	ld	$t0, 8($sp)		# secondary fd
	li	$t1, -1			# t1 = -1
	ld	$s1, ($sp)		# s1 = primary fd
	li	$v0, 5003		# SYS_close
	beq	$t0, $t2, .finish1	# secondary fd set?
	nop				# delay slot
	move	$a0, $t0		# secondary fd
	syscall				# syscall
	li	$v0, 5003		# SYS_close
.finish1:
	move	$a0, $s1		# primary fd
	syscall				# syscall
.jump:
	move	$v0, $zero		# rtld_fini
	ld	$t0, 8($s0)		# entry
	move	$sp, $s2		# restore stack pointer, delay slot
	jr	$t0			# enter
	nop				# delay slot

.auxvtab:
	.long	0			# 0
	.long	0			# 1
	.long	0			# 2
	.long	40			# 3 AT_PHDR
	.long	24			# 4 AT_PHENT
	.long	32			# 5 AT_PHNUM
	.long	0			# 6
	.long	48			# 7 AT_BASE
	.long	0			# 8
	.long	16			# 9 AT_ENTRY

.timespec:
	.quad	10
	.quad	10

# Local Variables:
# asm-comment-char: 35
# End:
