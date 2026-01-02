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

/* These "registers" alias a4-a7 and caution must be exercised not
   to overwrite them when issuing system calls.  */
define(`T4', `$a4')
define(`T5', `$a5')
define(`T6', `$a6')
define(`T7', `$a7')

	.set noreorder			# delay slots managed by hand
	.section .text
	.global __start
__start:
	## li	$v0, 5034		# SYS_nanosleep
	## dla	$a0, timespec		# rqtp
	## li	$a1, 0			# rmtp
	## syscall			# syscall
	ld	$s6, ($sp)		# original stack pointer
	DADDI3(	$s0, $sp, 16)		# start of load area
	DADDI2(	$sp, -16)		# primary fd, secondary fd
	li	$t0, -1			# secondary fd
	sd	$t0, 8($sp)		# initialize secondary fd
next_action:
	ld	$s1, ($s0)		# action number
	andi	$t0, $s1, 15		# t0 = action number & 15
	beqz	$t0, open_file		# open file?
	nop				# delay slot
	DADDI2(	$t0, -3)		# t0 -= 3
	beqz	$t0, rest_of_exec	# jump to code
	nop				# delay slot
	li	$t1, 1
	beq	$t0, $t1, do_mmap_anon	# anonymous mmap?
	nop				# delay slot
do_mmap:
	ld	$t0, 8($s0)		# vm address
	ld	$t1, 16($s0)		# file_offset
	ld	$t2, 24($s0)		# protection
	ld	$t3, 32($s0)		# length
	ld	$v0, 40($s0)		# flags
	ld	$v1, ($sp)		# primary fd
	andi	$s3, $s1, 16		# s1 & 16?
	beqz	$s3, do_mmap_1		# secondary fd?
	nop				# delay slot
	ld	$v1, 8($sp)		# secondary fd
do_mmap_1:
	move	$a0, $t0		# syscall arg
	move	$a1, $t3		# syscall arg
	move	$a2, $t2		# syscall arg
	move	$a3, $v0		# syscall arg
	move	$a4, $v1		# syscall arg
	move	$a5, $t1		# syscall arg
	li	$v0, 5009		# SYS_mmap
	syscall				# syscall
	bne	$a3, $zero, perror	# perror?
	nop				# delay slot
	ld	$t1, 48($s0)		# clear
	dadd	$t0, $a0, $a1		# t0 = end of mapping
	dsub	$t1, $t0, $t1		# t1 = t0 - clear
align:
	beq	$t0, $t1, continue	# already finished
	nop				# delay slot
	andi	$t2, $t1, 7		# t1 & 7?
	bnez	$t2, filld		# start filling longs
	nop				# delay slot
filld:
	dsub	$t2, $t0, $t1		# t2 = t0 - t1
	sltiu	$t2, $t2, 64		# t2 < 64?
	bne	$t2, $zero, fillb	# fill bytes
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
	j	filld			# fill either doubleword or byte
	nop				# delay slot
fillb:
	beq	$t0, $t1, continue	# already finished?
	nop				# delay slot
	sb	$zero, ($t1)		# clear byte
	DADDI2(	$t1, 1)			# t1++
continue:
	DADDI2(	$s0, 56)		# s0 = next action
	j	next_action		# next action
	nop				# delay slot
do_mmap_anon:
	ld	$t0, 8($s0)		# vm address
	ld	$t1, 16($s0)		# file_offset
	ld	$t2, 24($s0)		# protection
	ld	$t3, 32($s0)		# length
	ld	$v0, 40($s0)		# flags
	dli	$v1, -1			# fd
	j	do_mmap_1		# do mmap
	nop				# branch delay slot
open_file:
	dli	$v0, 5002		# SYS_open
	DADDI3(	$a0, $s0, 8)		# start of name
	move	$a1, $zero		# flags = O_RDONLY
	move	$a2, $zero		# mode = 0
	syscall				# syscall
	bne	$a3, $zero, perror	# perror
	nop				# delay slot
	DADDI2(	$s0, 8)			# start of string
	move	$t3, $s0		# t3 = s0
nextc:
	lb	$t0, ($s0)		# load byte
	DADDI2(	$s0, 1)			# s0++
	dli	$t1, 47			# directory separator `/'
	bne	$t0, $t1, nextc1	# is separator char?
	nop				# delay slot
	move	$t3, $s0		# t3 = char past separator
nextc1:
	bnez	$t0, nextc		# next character?
	nop				# delay slot
	DADDI2(	$s0, 7)			# adjust for round
	dli	$t2, -8			# t2 = -8
	and	$s0, $s0, $t2		# mask for round
	andi	$t0, $s1, 16		# t1 = s1 & 16
	move	$t1, $sp		# address of primary fd
	beqz	$t0, primary		# primary fd?
	nop				# delay slot
	DADDI2(	$t1, 8)			# address of secondary fd
	sd	$v0, ($t1)		# store fd
	j	next_action		# next action
	nop				# delay slot
primary:
	sd	$v0, ($t1)		# store fd
	dli	$v0, 5153		# SYS_prctl
	dli	$a0, 15			# PR_SET_NAME
	move	$a1, $t3		# char past separator
	move	$a2, $zero		# a2
	move	$a3, $zero		# a3
	move	$a4, $zero		# a4
	move	$a5, $zero		# a5
	syscall				# syscall
	j	next_action		# next action
	nop				# delay slot
perror:
	move	$a0, $v0		# errno
	dli	$v0, 5058		# SYS_exit
	syscall				# syscall
rest_of_exec:
	move	$s1, $s6		# original SP
	ld	$t0, ($s1)		# argc
	dsll	$t0, $t0, 3		# argc *= 8
	DADDI2(	$t0, 16)		# argc += 16
	dadd	$s1, $s1, $t0		# s1 = start of envp
skip_environ:
	/* Locate the auxiliary vector.  */
	li	$t8, 8 			# DADDI2 isn't appropriate in delay slots.
1:	ld	$t0, ($s1)		# t0 = *s1
	bnez	$t0, 1b			# skip environment entry
	dadd	$s1, $s1, $t8		# s1++
	move	$s2, $s1		# s2 = end of environment
	li	$t8, 16
1:	ld	$t0, ($s1)		# t0 = s1->a_type
	bnez	$t0, 1b			# skip auxiliary vector entry
	dadd	$s1, $s1, $t8		# (Elf64_auxv_t *) s1++
	/* Decide how many bytes must be copied and where to
	   save the file name.  Move the stack pointer to a safe
	   position below any data that must be preserved.  */
	ld	$t1, 56($s0)		# length of string
	DADDI2(	$t1, 1)
	DADDI3(	$t2, $s0, 64)		# pointer to string
	dsub	$t3, $s1, $s6		# number of bytes in vectors
	dsub	$t0, $s1, $t1		# position of string
	and	$t0, $t0, -16		# align value
	dsub	$t3, $t0, $t3		# position of argc
	and	$t3, $t3, -16		# align value
	/* Move the stack pointer and save required information.
	   8($fp)   = secondary/interpreter fd.
	   0($fp)   = primary/executable fd.
	   -8($fp)  = cmd->entry
	   -16($fp) = cmd->at_entry
	   -24($fp) = cmd->at_phent
	   -32($fp) = cmd->at_phnum
	   -40($fp) = cmd->at_phdr
	   -48($fp) = cmd->at_base
	$sp = copy of string.  */
	move	T4, $sp			# current sp
	dsub	T5, $t3, $sp		# new argc - current sp
	li	$t8, -16
	blt	T5, 16, 1f		# more than two slots apart
	dadd	$sp, $t3, $t8		# $sp = two slots below new argc
	j	2f			# skip copying fds
	move	$sp, T4			# retain current sp
1:	ld	T5, (T4)		# old primary fd
	ld	T5, ($sp)		# save the same
	ld	T5, 8(T4)		# old interpreter fd
	sd	T5, 8($sp)		# save the same
2:	move	$fp, $sp		# set base pointer
	DADDI2(	$sp, -48)		# command data
	ld	T5, 8($s0)		# entry
	ld	T6, 16($s0)		# at_entry
	ld	T7, 24($s0)		# at_phent
	ld	$t8, 32($s0)		# at_phnum
	sd	T5, -8($fp)		# save entry
	ld	T5, 40($s0)		# at_phdr
	sd	T6, -16($fp)		# save at_entry
	ld	T6, 48($s0)		# at_base
	sd	T7, -24($fp)		# save at_phent
	sd	$t8, -32($fp)		# save at_phnum
	sd	T5, -40($fp)		# save at_phdr
	sd	T6, -48($fp)		# save at_base
	dsub	$sp, $sp, $t1		# space for string
	/* Save the input string.  */
	dadd	T5, $t2, $t1		# end of source ($t2)
	move	T6, $sp			# dst
	move	$s0, $t1		# $s0 = length of string
	/* src = $t2, dst = T6 */
	bgeu	$t2, T5, 2f		# there already?
	nop
1:	lb	$t1, ($t2)		# $t1 = *$t2
	DADDI2(	$t2, 1)			# $t2++
	DADDI2(	T6, 1)			# $t6++
	bltu	$t2, T5, 1b
	sb	$t1, -1(T6)		# *(T6 - 1) = $t1
2:	move	$s3, $sp		# copy of string
	and	$sp, $sp, -16		# align stack
copy_env_and_args:
	/* Copy argc, argv, and the environment array.
	   T4 = destination, T5 = src, $s2 = src_end  */
	move	T4, $t3			# destination of argc
	move	T5, $s6			# original SP
	bgeu	T5, $s2, 2f		# there already?
	nop
1:	ld	$t1, (T5)		# $t1 = *src
	DADDI2(	T5, 8)			# src++
	DADDI2(	T4, 8)			# dst++
	bltu	T5, $s2, 1b		# src < src_end
	sd	$t1, -8(T4)		# *(dst - 8) = $t1
copy_auxv:
	/* T4 = destination, T5 = first auxval.  */
2:	ld	$t1, (T5)		# a_type
	ld	$t2, 8(T5)		# a_un.a_val
	DADDI2(	T4, 16)			# (Elf64_auxv_t *) dst++
	DADDI2(	T5, 16)			# (Elf64_auxv_t *) src
	beqz	$t1, 8f			# AT_NULL
	li	T6, 3
	beq	$t1, T6, 1f		# AT_PHDR
	li	T6, 4
	beq	$t1, T6, 2f		# AT_PHENT
	li	T6, 5
	beq	$t1, T6, 3f		# AT_PHNUM
	li	T6, 9
	beq	$t1, T6, 4f		# AT_ENTRY
	li	T6, 7
	beq	$t1, T6, 5f		# AT_BASE
	li	T6, 31
	beq	$t1, T6, 6f		# AT_EXECFN
	nop
	b	7f
	nop
1:	b	7f
	ld	$t2, -40($fp)
2:	b	7f
	ld	$t2, -24($fp)
3:	b	7f
	ld	$t2, -32($fp)
4:	b	7f
	ld	$t2, -16($fp)
5:	b	7f
	ld	$t2, -48($fp)
6:	b	7f
	move	$t2, $t0
7:	sd	$t1, -16(T4)		# dst->a_type
	j	copy_auxv
	sd	$t2, -8(T4)		# dst->a_un.a_val
	/* Copy the final element.  */
8:	sd	$t1, -16(T4)		# dst->a_type
	sd	$t2, -8(T4)		# dst->a_un.a_val
finish:
	/* Copy the string to its position in auxv
	   (src = $s3, dst = $t0).  */
	dadd	$t1, $s3, $s0		# src end
	bgeu	$s3, $t1, 2f		# there already?
	nop
1:	lb	$t2, ($s3)		# c = *src
	DADDI2(	$s3, 1)			# src++
	DADDI2(	$t0, 1)			# dst++
	bltu	$s3, $t1, 1b
	sb	$t2, -1($t0)	   	# *(dst - 1) = c
	/* Save variables.  */
2:	move	$s6, $t3		# new stack pointer
	ld	$t0, 8($fp)		# secondary fd
	li	$t1, -1			# t1 = -1
	ld	$s1, ($fp)		# s1 = primary fd
	beq	$t0, $t2, finish1	# secondary fd set?
	li	$v0, 5003		# SYS_close
	move	$a0, $t0		# secondary fd
	syscall				# syscall
	li	$v0, 5003		# SYS_close
finish1:
	move	$a0, $s1		# primary fd
	syscall				# syscall
jump:
	move	$v0, $zero		# rtld_fini
	ld	$t9, -8($fp)		# entry
	move	$sp, $s6		# restore stack pointer, delay slot
        /* Clear at least one page's worth of stack.  glibc on mipsel
	   copies certain fields from the stack to the `link_map'
	   structure representing ld.so, which are not subsequently
	   replaced if otherwise than zero.

	   XXX: report this glibc bug?  */
	DADDI3(	$v0, $sp, -4096)
	and	$v0, $v0, -4095
1:	sd	$zero, ($v0)		# copy 32 byte blocks
	sd	$zero, 8($v0)
	sd	$zero, 16($v0)
	sd	$zero, 24($v0)
	DADDI2(	$v0, 32)
	dsub	$t0, $sp, $v0		# remainder
	bge	$t0, 32, 1b		# test remainder
	nop				# copy 4 byte blocks
	beqz	$t0, 2f
	nop
1:	DADDI2(	$v0, 4)
	bltu	$v0, $sp, 1b
	sw	$zero, -4($v0)
2:	jr	$t9			# enter
	nop				# delay slot

## timespec:
## 	.quad	10
## 	.quad	10

# Local Variables:
# asm-comment-char: ?#
# End:
