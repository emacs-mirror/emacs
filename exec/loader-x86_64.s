define(`CC', `
dnl')

CC Copyright (C) 2023-2024 Free Software Foundation, Inc.
CC
CC This file is part of GNU Emacs.
CC
CC GNU Emacs is free software: you can redistribute it and/or modify
CC it under the terms of the GNU General Public License as published
CC by the Free Software Foundation, either version 3 of the License,
CC or (at your option) any later version.
CC
CC GNU Emacs is distributed in the hope that it will be useful, but
CC WITHOUT ANY WARRANTY; without even the implied warranty of
CC MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
CC General Public License for more details.
CC
CC You should have received a copy of the GNU General Public License
CC along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

	.section .text
	.global _start
_start:
dnl	movq	$35, %rax		CC SYS_nanosleep
dnl	leaq	timespec(%rip), %rdi
dnl	xorq	%rsi, %rsi
dnl	syscall
	popq	%r13			CC original SP
	popq	%r15			CC size of load area.
	movq	$-1, %r12		CC r12 is the interpreter fd
.next_action:
	movq	(%rsp), %r14		CC action number
	movq	%r14, %r15		CC original action number
	andq	$-17, %r14
	cmpq	$0, %r14		CC open file?
	je	.open_file
	cmpq	$3, %r14 		CC jump?
	je	.rest_of_exec
	cmpq	$4, %r14		CC anonymous mmap?
	je	.do_mmap_anon
.do_mmap:
	movq	$9, %rax		CC SYS_mmap
	movq	8(%rsp), %rdi		CC address
	movq	16(%rsp), %r9		CC offset
	movq	24(%rsp), %rdx		CC protection
	movq	32(%rsp), %rsi		CC length
	movq	40(%rsp), %r10		CC flags
					CC set r8 to the primary fd unless r15 & 16
	testq	$16, %r15
	movq	%r12, %r8
	cmovzq	%rbx, %r8
.do_mmap_1:
	syscall
	cmpq	$-1, %rax		CC mmap failed
	je	.perror
	movq	48(%rsp), %r9		CC clear
	testq	%r9, %r9
	jz	.continue
	movq	8(%rsp), %r10		CC start of mapping
	addq	32(%rsp), %r10		CC end of mapping
	subq	%r9, %r10		CC start of clear area
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
	movq	$9, %rax		CC SYS_mmap
	movq	8(%rsp), %rdi		CC address
	movq	16(%rsp), %r9		CC offset
	movq	24(%rsp), %rdx		CC protection
	movq	32(%rsp), %rsi		CC length
	movq	40(%rsp), %r10		CC flags
	movq	$-1, %r8		CC -1
	jmp	.do_mmap_1
.open_file:
	movq	$2, %rax		CC SYS_open
	leaq	8(%rsp), %rdi		CC rdi = %rsp + 8
	xorq	%rsi, %rsi		CC flags = O_RDONLY
	xorq	%rdx, %rdx		CC mode = 0
	syscall
	cmpq	$-1, %rax		CC open failed
	jle	.perror
	movq	%rdi, %rsp		CC rsp = start of string
	subq	$1, %rsp
	movq	%rsp, %r14		CC r14 = start of string
.nextc:
	addq	$1, %rsp
	movb	(%rsp), %dil		CC rdi = *rsp
	cmpb	$47, %dil		CC *rsp == '/'?
	jne	.nextc1
	movq	%rsp, %r14		CC r14 = rsp
	addq	$1, %r14		CC r14 = char past separator
.nextc1:
	cmpb	$0, %dil		CC *rsp == 0?
	jne	.nextc
	addq	$8, %rsp		CC adjust past rsp prior to rounding
	andq	$-8, %rsp		CC round rsp up to the next quad
	testq	$16, %r15		CC r15 & 16?
	jz	.primary
	movq	%rax, %r12		CC otherwise, move fd to r12
	jmp	.next_action
.primary:
	movq	%rax, %rbx		CC if not, move fd to rbx
	movq	$157, %rax		CC SYS_prctl
	movq	$15, %rdi		CC PR_SET_NAME
	movq	%r14, %rsi		CC arg1
	xorq	%rdx, %rdx		CC arg2
	xorq	%r10, %r10		CC arg3
	xorq	%r8, %r8		CC arg4
	xorq	%r9, %r9		CC arg5
	syscall
	jmp	.next_action
.perror:
	movq	%rax, %r12		CC error code
	negq	%r12
	movq	$1, %rax		CC SYS_write
	movq	$1, %rdi		CC stdout
	leaq	error(%rip), %rsi	CC buffer
	movq	$23, %rdx		CC count
	syscall
	movq	$60, %rax		CC SYS_exit
	movq	%r12, %rdi		CC code
	syscall
.rest_of_exec:				CC rsp now points to six quads:
	movq	%rsp, %r8		CC now, they are r8
	movq	%r13, %rsp		CC restore SP
	popq	%r10			CC argc
	leaq	8(%rsp,%r10,8), %rsp	CC now at start of environ
.skip_environ:
	popq	%r10			CC envp[N]
	testq	%r10, %r10		CC envp[n]?
	jnz	.skip_environ		CC otherwise, rsp is now at the start of auxv
.one_auxv:
	popq	%rcx			CC auxv type
	addq	$8, %rsp		CC skip value
	testq	%rcx, %rcx		CC is 0?
	jz	.cleanup
	cmpq	$3, %rcx		CC is AT_PHDR?
	je	.replace_phdr
	cmpq	$4, %rcx		CC is AT_PHENT?
	je	.replace_phent
	cmpq	$5, %rcx		CC is AT_PHNUM?
	je	.replace_phnum
	cmpq	$9, %rcx		CC is AT_ENTRY?
	je	.replace_entry
	cmpq	$7, %rcx		CC is AT_BASE?
	je	.replace_base
	jmp	.one_auxv
.replace_phdr:
	movq	40(%r8), %r9
	movq	%r9, -8(%rsp)		CC set at_phdr
	jmp	.one_auxv
.replace_phent:
	movq	24(%r8), %r9
	movq	%r9, -8(%rsp)		CC set at_phent
	jmp	.one_auxv
.replace_phnum:
	movq	32(%r8), %r9
	movq	%r9, -8(%rsp)		CC set at_phnum
	jmp	.one_auxv
.replace_entry:
	movq	16(%r8), %r9
	movq	%r9, -8(%rsp)		CC set at_entry
	jmp	.one_auxv
.replace_base:
	movq	48(%r8), %r9
	movq	%r9, -8(%rsp)		CC set at_base
	jmp	.one_auxv
.cleanup:
	movq	$3, %rax		CC SYS_close
	cmpq	$-1, %r12		CC see if interpreter fd is set
	je	.cleanup_1
	movq	%r12, %rdi
	syscall
	movq	$3, %rax		CC SYS_close
.cleanup_1:
	movq	%rbx, %rdi
	syscall
.enter:
	pushq	$0
	popfq				CC clear FP state
	movq	%r13, %rsp		CC restore SP
	xorq	%rdx, %rdx		CC clear rtld_fini
	jmpq	*8(%r8)			CC entry

error:
	.ascii	"_start: internal error."
timespec:
	.quad	10
	.quad	10
