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
#	movl	$162, %eax		# SYS_nanosleep
#	leal	timespec, %ebx
#	xorl	%ecx, %ecx
#	int	$0x80
	leal	8(%esp), %ebp		# ebp = start of load area
	subl	$8, %esp		# (%esp) = primary fd, 4(%esp) = secondary fd
	movl	$-1, 4(%esp)
.next_action:
	movl	(%ebp), %edx		# edx = action number
	andl	$-17, %edx
	cmpl	$0, %edx		# open file?
	je	.open_file
	cmpl	$3, %edx		# jump?
	je	.rest_of_exec
	cmpl	$4, %edx		# anonymous mmap?
	je	.do_mmap_anon
.do_mmap:
	subl	$24, %esp
	movl	$90, %eax		# SYS_old_mmap
	movl	%esp, %ebx
	movl	4(%ebp), %ecx		# address
	movl	%ecx, (%esp)
	movl	16(%ebp), %ecx		# length
	movl	%ecx, 4(%esp)
	movl	12(%ebp), %ecx		# protection
	movl	%ecx, 8(%esp)
	movl	20(%ebp), %ecx		# flags
	movl	%ecx, 12(%esp)
	testl	$16, (%ebp)		# primary?
	movl	28(%esp), %ecx
	cmovzl	24(%esp), %ecx
	movl	%ecx, 16(%esp)		# fd
	movl	8(%ebp), %ecx		# offset
	movl	%ecx, 20(%esp)
.do_mmap_1:
	int	$0x80
	addl	$24, %esp		# restore esp
	cmpl	$-1, %eax		# mmap failed?
	je	.perror
	movl	24(%ebp), %ecx		# clear
	testl	%ecx, %ecx
	jz	.continue
	movl	4(%ebp), %esi		# start of mapping
	addl	16(%ebp), %esi		# end of mapping
	subl	%ecx, %esi		# start of clear area
.again:
	testl	%ecx, %ecx
	jz	.continue
	subl	$1, %ecx
	movb	$0, (%esi, %ecx, 1)
	jmp	.again
.continue:
	leal	28(%ebp), %ebp
	jmp	.next_action
.do_mmap_anon:
	subl	$24, %esp
	movl	$90, %eax		# SYS_old_mmap
	movl	%esp, %ebx
	movl	4(%ebp), %ecx		# address
	movl	%ecx, (%esp)
	movl	16(%ebp), %ecx		# length
	movl	%ecx, 4(%esp)
	movl	12(%ebp), %ecx		# protection
	movl	%ecx, 8(%esp)
	movl	20(%ebp), %ecx		# flags
	movl	%ecx, 12(%esp)
	movl	$-1, 16(%esp)		# fd
	movl	8(%ebp), %ecx		# offset
	movl	%ecx, 20(%esp)
	jmp	.do_mmap_1
.open_file:
	movl	$5, %eax		# SYS_open
	leal	4(%ebp), %ebx		# ebx = %esp + 8
	pushl	%ebx
	xorl	%ecx, %ecx		# flags = O_RDONLY
	xorl	%edx, %edx		# mode = 0
	int	$0x80
	cmpl	$-1, %eax		# open failed?
	jle	.perror
	movl	%ebp, %esi		# (esi) = original action number
	popl	%ebp			# ebp = start of string
	movl	%ebp, %ecx		# char past separator
	decl	%ebp
.nextc:
	incl	%ebp
	movb	(%ebp), %dl		# dl = *ebp
	cmpb	$47, %dl		# dl == '\?'?
	jne	.nextc1
	leal	1(%ebp), %ecx		# ecx = char past separator
.nextc1:
	cmpb	$0, %dl			# dl == 0?
	jne	.nextc
	addl	$4, %ebp		# adjust past ebp prior to rounding
	andl	$-4, %ebp		# round ebp up to the next long
	testl	$16, (%esi)		# original action number & 16?
	jz	.primary
	movl	%eax, 4(%esp)		# secondary fd = eax
	jmp	.next_action
.primary:
	pushl	%ebp
	xorl	%esi, %esi		# arg3
	movl	%eax, 4(%esp)		# primary fd = eax
	xorl	%edx, %edx		# arg2
	movl	$15, %ebx		# PR_SET_NAME, arg1 = ecx
	xorl	%edi, %edi		# arg4
	movl	$172, %eax		# SYS_prctl
	xorl	%ebp, %ebp		# arg5
	int	$0x80			# syscall
	popl	%ebp
	jmp	.next_action
.perror:
	movl	%eax, %ebx
	negl	%ebx
	movl	$1, %eax
	int	$0x80
.rest_of_exec:
	movl	8(%esp), %ecx		# ecx = original stack pointer
	movl	(%ecx), %esi		# esi = argc
	leal	8(%ecx, %esi, 4), %ecx	# ecx = start of environ
.skip_environ:
	movl	(%ecx), %esi		# envp[N]
	addl	$4, %ecx
	testl	%esi, %esi		# envp[n] ?
	jnz	.skip_environ		# otherwise, esi is now at the start of auxv
.one_auxv:
	movl	(%ecx), %esi		# auxv type
	leal	8(%ecx), %ecx		# skip to next auxv
	testl	%esi, %esi		# is 0?
	jz	.cleanup
	cmpl	$3, %esi		# is AT_PHDR
	je	.replace_phdr
	cmpl	$4, %esi		# is AT_PHENT?
	je	.replace_phent
	cmpl	$5, %esi		# is AT_PHNUM?
	je	.replace_phnum
	cmpl	$9, %esi		# is AT_ENTRY?
	je	.replace_entry
	cmpl	$7, %esi		# is AT_BASE
	je	.replace_base
	jmp	.one_auxv
.replace_phdr:
	movl	20(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	.one_auxv
.replace_phent:
	movl	12(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	.one_auxv
.replace_phnum:
	movl	16(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	.one_auxv
.replace_entry:
	movl	8(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	.one_auxv
.replace_base:
	movl	24(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	.one_auxv
.cleanup:
	movl	$6, %eax		# SYS_close
	cmpl	$-1, 4(%esp)		# see if interpreter fd is set
	je	.cleanup_1
	movl	4(%esp), %ebx
	int 	$0x80
	movl	$6, %eax		# SYS_close
.cleanup_1:
	movl	(%esp), %ebx
	int	$0x80
.enter:
	pushl	$0
	popfl				# restore floating point state
	movl	8(%esp), %esp		# restore initial stack pointer
	xorl	%edx, %edx		# clear rtld_fini
	jmpl	*4(%ebp)		# entry

timespec:
	.long	10
	.long	10
