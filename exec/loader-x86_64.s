# Copyright (C) 2023-2024 Free Software Foundation, Inc.
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
.next_action:
	movq	(%rsp), %r14		# action number
	movq	%r14, %r15		# original action number
	andq	$-17, %r14
	cmpq	$0, %r14		# open file?
	je	.open_file
	cmpq	$3, %r14 		# jump?
	je	.rest_of_exec
	cmpq	$4, %r14		# anonymous mmap?
	je	.do_mmap_anon
.do_mmap:
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
.do_mmap_1:
	syscall
	cmpq	$-1, %rax		# mmap failed
	je	.perror
	movq	48(%rsp), %r9		# clear
	testq	%r9, %r9
	jz	.continue
	movq	8(%rsp), %r10		# start of mapping
	addq	32(%rsp), %r10		# end of mapping
	subq	%r9, %r10		# start of clear area
.again:
	testq	%r9, %r9
	jz	.continue
	subq	$1, %r9
	movb	$0, (%r10, %r9, 1)
	jmp	.again
.continue:
	leaq	56(%rsp), %rsp
	jmp	.next_action
.do_mmap_anon:
	movq	$9, %rax		# SYS_mmap
	movq	8(%rsp), %rdi		# address
	movq	16(%rsp), %r9		# offset
	movq	24(%rsp), %rdx		# protection
	movq	32(%rsp), %rsi		# length
	movq	40(%rsp), %r10		# flags
	movq	$-1, %r8		# -1
	jmp	.do_mmap_1
.open_file:
	movq	$2, %rax		# SYS_open
	leaq	8(%rsp), %rdi		# rdi = %rsp + 8
	xorq	%rsi, %rsi		# flags = O_RDONLY
	xorq	%rdx, %rdx		# mode = 0
	syscall
	cmpq	$-1, %rax		# open failed
	jle	.perror
	movq	%rdi, %rsp		# rsp = start of string
	subq	$1, %rsp
	movq	%rsp, %r14		# r14 = start of string
.nextc:
	addq	$1, %rsp
	movb	(%rsp), %dil		# rdi = *rsp
	cmpb	$47, %dil		# *rsp == '/'?
	jne	.nextc1
	movq	%rsp, %r14		# r14 = rsp
	addq	$1, %r14		# r14 = char past separator
.nextc1:
	cmpb	$0, %dil		# *rsp == 0?
	jne	.nextc
	addq	$8, %rsp		# adjust past rsp prior to rounding
	andq	$-8, %rsp		# round rsp up to the next quad
	testq	$16, %r15		# r15 & 16?
	jz	.primary
	movq	%rax, %r12		# otherwise, move fd to r12
	jmp	.next_action
.primary:
	movq	%rax, %rbx		# if not, move fd to rbx
	movq	$157, %rax		# SYS_prctl
	movq	$15, %rdi		# PR_SET_NAME
	movq	%r14, %rsi		# arg1
	xorq	%rdx, %rdx		# arg2
	xorq	%r10, %r10		# arg3
	xorq	%r8, %r8		# arg4
	xorq	%r9, %r9		# arg5
	syscall
	jmp	.next_action
.perror:
	movq	%rax, %r12		# error code
	negq	%r12
	movq	$1, %rax		# SYS_write
	movq	$1, %rdi		# stdout
	leaq	error(%rip), %rsi	# buffer
	movq	$23, %rdx		# count
	syscall
	movq	$60, %rax		# SYS_exit
	movq	%r12, %rdi		# code
	syscall
.rest_of_exec:				# rsp now points to six quads:
	movq	%rsp, %r8		# now, they are r8
	movq	%r13, %rsp		# restore SP
	popq	%r10			# argc
	leaq	8(%rsp,%r10,8), %rsp	# now at start of environ
.skip_environ:
	popq	%r10			# envp[N]
	testq	%r10, %r10		# envp[n]?
	jnz	.skip_environ		# otherwise, rsp is now at the start of auxv
.one_auxv:
	popq	%rcx			# auxv type
	addq	$8, %rsp		# skip value
	testq	%rcx, %rcx		# is 0?
	jz	.cleanup
	cmpq	$3, %rcx		# is AT_PHDR?
	je	.replace_phdr
	cmpq	$4, %rcx		# is AT_PHENT?
	je	.replace_phent
	cmpq	$5, %rcx		# is AT_PHNUM?
	je	.replace_phnum
	cmpq	$9, %rcx		# is AT_ENTRY?
	je	.replace_entry
	cmpq	$7, %rcx		# is AT_BASE?
	je	.replace_base
	jmp	.one_auxv
.replace_phdr:
	movq	40(%r8), %r9
	movq	%r9, -8(%rsp)		# set at_phdr
	jmp	.one_auxv
.replace_phent:
	movq	24(%r8), %r9
	movq	%r9, -8(%rsp)		# set at_phent
	jmp	.one_auxv
.replace_phnum:
	movq	32(%r8), %r9
	movq	%r9, -8(%rsp)		# set at_phnum
	jmp	.one_auxv
.replace_entry:
	movq	16(%r8), %r9
	movq	%r9, -8(%rsp)		# set at_entry
	jmp	.one_auxv
.replace_base:
	movq	48(%r8), %r9
	movq	%r9, -8(%rsp)		# set at_base
	jmp	.one_auxv
.cleanup:
	movq	$3, %rax		# SYS_close
	cmpq	$-1, %r12		# see if interpreter fd is set
	je	.cleanup_1
	movq	%r12, %rdi
	syscall
	movq	$3, %rax		# SYS_close
.cleanup_1:
	movq	%rbx, %rdi
	syscall
.enter:
	pushq	$0
	popfq				# clear FP state
	movq	%r13, %rsp		# restore SP
	xorq	%rdx, %rdx		# clear rtld_fini
	jmpq	*8(%r8)			# entry

error:
	.ascii	"_start: internal error."
timespec:
	.quad	10
	.quad	10
