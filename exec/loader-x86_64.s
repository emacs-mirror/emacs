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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

	.section .text
	.global _start
_start:
#	movq	$35, %rax		# SYS_nanosleep
#	leaq	timespec(%rip), %rdi
#	xorq	%rsi, %rsi
#	syscall
	popq	%r13			# original SP
	popq	%r15			# size of load area.
	movq	$-1, %r12		# r12 is the interpreter fd
next_action:
	movq	(%rsp), %r14		# action number
	movq	%r14, %r15		# original action number
	andq	$-17, %r14
	cmpq	$0, %r14		# open file?
	je	open_file
	cmpq	$3, %r14 		# jump?
	je	rest_of_exec
	cmpq	$4, %r14		# anonymous mmap?
	je	do_mmap_anon
do_mmap:
	movq	$9, %rax		# SYS_mmap
	movq	8(%rsp), %rdi		# address
	movq	16(%rsp), %r9		# offset
	movq	24(%rsp), %rdx		# protection
	movq	32(%rsp), %rsi		# length
	movq	40(%rsp), %r10		# flags
					# set r8 to the primary fd unless r15 & 16
	testq	$16, %r15
	movq	%r12, %r8
	cmovzq	%rbx, %r8
do_mmap_1:
	syscall
	cmpq	$-1, %rax		# mmap failed
	je	perror
	movq	48(%rsp), %r9		# clear
	testq	%r9, %r9
	jz	continue
	movq	8(%rsp), %r10		# start of mapping
	addq	32(%rsp), %r10		# end of mapping
	subq	%r9, %r10		# start of clear area
again:
	testq	%r9, %r9
	jz	continue
	subq	$1, %r9
	movb	$0, (%r10, %r9, 1)
	jmp	again
continue:
	leaq	56(%rsp), %rsp
	jmp	next_action
do_mmap_anon:
	movq	$9, %rax		# SYS_mmap
	movq	8(%rsp), %rdi		# address
	movq	16(%rsp), %r9		# offset
	movq	24(%rsp), %rdx		# protection
	movq	32(%rsp), %rsi		# length
	movq	40(%rsp), %r10		# flags
	movq	$-1, %r8		# -1
	jmp	do_mmap_1
open_file:
	movq	$2, %rax		# SYS_open
	leaq	8(%rsp), %rdi		# rdi = %rsp + 8
	xorq	%rsi, %rsi		# flags = O_RDONLY
	xorq	%rdx, %rdx		# mode = 0
	syscall
	cmpq	$-1, %rax		# open failed
	jle	perror
	movq	%rdi, %rsp		# rsp = start of string
	subq	$1, %rsp
	movq	%rsp, %r14		# r14 = start of string
nextc:
	addq	$1, %rsp
	movb	(%rsp), %dil		# rdi = *rsp
	cmpb	$47, %dil		# *rsp == '/'?
	jne	nextc1
	movq	%rsp, %r14		# r14 = rsp
	addq	$1, %r14		# r14 = char past separator
nextc1:
	cmpb	$0, %dil		# *rsp == 0?
	jne	nextc
	addq	$8, %rsp		# adjust past rsp prior to rounding
	andq	$-8, %rsp		# round rsp up to the next quad
	testq	$16, %r15		# r15 & 16?
	jz	primary
	movq	%rax, %r12		# otherwise, move fd to r12
	jmp	next_action
primary:
	movq	%rax, %rbx		# if not, move fd to rbx
	movq	$157, %rax		# SYS_prctl
	movq	$15, %rdi		# PR_SET_NAME
	movq	%r14, %rsi		# arg1
	xorq	%rdx, %rdx		# arg2
	xorq	%r10, %r10		# arg3
	xorq	%r8, %r8		# arg4
	xorq	%r9, %r9		# arg5
	syscall
	jmp	next_action
perror:
	movq	%rax, %r12		# error code
	negq	%r12
	movq	$1, %rax		# SYS_write
	movq	$1, %rdi		# stdout
	leaq	error(%rip), %rsi	# buffer
	movq	$24, %rdx		# count
	syscall
	movq	$60, %rax		# SYS_exit
	movq	%r12, %rdi		# code
	syscall
rest_of_exec:				# rsp now points to seven quads + string:
	movq	%rsp, %r8		# now, they are r8
	movq	%r13, %rsp		# restore SP
	popq	%r10			# argc
	leaq	8(%rsp,%r10,8), %rsp	# now at start of environ
skip_environ:
	popq	%rcx			# envp[N]
	testq	%rcx, %rcx		# envp[n]?
	jnz	skip_environ		# otherwise, rsp is now at the end of auxv
	movq	%rsp, %r11		# start of auxv
1:	testq	$-1, (%r11)		# NULL?
	leaq	16(%r11), %r11		# next entry
	jnz	1b			# otherwise copy auxv
	/* Prepare sufficient space for the new executable name at the
	   start of the auxiliary vector.  */
1:	leaq	64(%r8), %rsi		# file name
	movq	56(%r8), %r9		# name length
	leaq	-1(%r11), %r14
	subq	%r9, %r14		# destination of file name
	andq	$-16, %r14		# align destination
	/* Prepare to copy argv, environ and auxv.  */
1:	subq	%r13, %r11		# size required
	addq	$15, %r11		# align size
	andq	$-16, %r11
	negq	%r11			# subtract
	leaq	-56(%r14,%r11,1), %r11	# %r11 = destination - struct exec_jump_command
	/* Move the file name out of the way.  */
	leaq	9(%rsi,%r9,1), %r10	# end of name + 8
	cmpq	%r10, %r11		# end of name >= struct exec_jump_command - 8
	jae	1f			# save exec command
	xorq	%r10, %r10
	subq	%r9, %r10
	leaq	-9(%r11,%r10,1), %rdi	# position of new name
	movq	%rdi, %r10
	cld
	leaq	1(%r9), %rcx		# length (including termination)
  rep	movsb				# copy file name
	movq	%r10, %rsi		# file name
	/* Preserve jump command.  */
1:	cmpq	%r8, %r11		# decide copy direction
	jb	1f			# copy forward
	movq	48(%r8), %rax
	movq	%rax, 48(%r11)		# %r11->at_base
	movq	40(%r8), %rax
	movq	%rax, 40(%r11)		# %r11->at_phdr
	movq	32(%r8), %rax
	movq	%rax, 32(%r11)		# %r11->at_phnum
	movq	24(%r8), %rax
	movq	%rax, 24(%r11)		# %r11->at_phent
	movq	16(%r8), %rax
	movq	%rax, 16(%r11)		# %r11->at_entry
	movq	8(%r8), %rax
	movq	%rax, 8(%r11)		# %r11->entry
	movq	(%r8), %rax
	movq	%rax, (%r11)		# %r11->command
	movq	%r14, -8(%r11)		# destination of file name
	jmp	copy_env_and_args
1:	movq	%r14, -8(%r11)		# destination of file name
	movq	(%r8), %rax
	movq	%rax, (%r11)		# %r11->command
	movq	8(%r8), %rax
	movq	%rax, 8(%r11)		# %r11->entry
	movq	16(%r8), %rax
	movq	%rax, 16(%r11)		# %r11->at_entry
	movq	24(%r8), %rax
	movq	%rax, 24(%r11)		# %r11->at_phent
	movq	32(%r8), %rax
	movq	%rax, 32(%r11)		# %r11->at_phnum
	movq	40(%r8), %rax
	movq	%rax, 40(%r11)		# %r11->at_phdr
	movq	48(%r8), %rax
	movq	%rax, 48(%r11)		# %r11->at_base
copy_env_and_args:
	/* Copy argv and environ to their new positions.  */
	leaq	8(%r13), %r10		# src
	leaq	64(%r11), %rdi		# dest
	movq	(%r13), %rcx		# argc
	movq	%rcx, -8(%rdi)		# copy argc
1:	movq	(%r10), %rcx
	movq	%rcx, (%rdi)
	testq	%rcx, %rcx
	leaq	8(%r10), %r10		# src++
	leaq	8(%rdi), %rdi		# dst++
	jnz	1b
1:	movq	(%r10), %rcx
	movq	%rcx, (%rdi)
	testq	%rcx, %rcx
	leaq	8(%r10), %r10		# src++
	leaq	8(%rdi), %rdi		# dst++
	jnz	1b
copy_auxv:
	movq	(%r10), %rcx		# a_type
	movq	8(%r10), %rdx		# a_un.a_val
	addq	$16, %r10		# next entry
	movq	%rcx, (%rdi)
	jrcxz	cleanup			# AT_NULL
	cmpq	$3, %rcx		# AT_PHDR
	cmoveq	40(%r11), %rdx		# %r11->at_phdr
	cmpq	$4, %rcx		# AT_PHENT
	cmoveq	24(%r11), %rdx		# %r11->at_phent
	cmpq	$5, %rcx		# AT_PHNUM
	cmoveq	32(%r11), %rdx		# %r11->at_phnum
	cmpq	$9, %rcx		# AT_ENTRY
	cmoveq	16(%r11), %rdx		# %r11->at_entry
	cmpq	$7, %rcx		# AT_BASE
	cmoveq	48(%r11), %rdx		# %r11->at_base
	cmpq	$31, %rcx		# AT_EXECFN
	jne	1f
	movq	-8(%r11), %rdx		# string
1:	movq	%rdx, 8(%rdi)		# AT_NULL value
	addq	$16, %rdi		# next entry
	jmp	copy_auxv
cleanup:
	/* Copy the filename.  */
	movq	-8(%r11), %rdi		# destination of file name
	leaq	1(%r9), %rcx		# length (including termination)
  rep	movsb
	movq	%rdx, 8(%rdi)		# AT_NULL value
	leaq	56(%r11), %r13		# restore original stack pointer
	movq	$3, %rax		# SYS_close
	cmpq	$-1, %r12		# see if interpreter fd is set
	je	cleanup_1
	movq	%r12, %rdi
	syscall
	movq	$3, %rax		# SYS_close
cleanup_1:
	movq	%rbx, %rdi
	syscall
	/* Enter the program.  */
	pushq	$0
	popfq				# clear FP state
	movq	%r13, %rsp		# restore SP
	xorq	%rdx, %rdx		# clear rtld_fini
	jmpq	*-48(%rsp)		# entry

error:
	.ascii	"_start: internal error.\n"
#timespec:
#	.quad	10
#	.quad	10

# Local Variables:
# asm-comment-char: ?#
# End:
