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
dnl	movl	$162, %eax		CC SYS_nanosleep
dnl	leal	timespec, %ebx
dnl	xorl	%ecx, %ecx
dnl	int	$0x80
	leal	8(%esp), %ebp		CC ebp = start of load area
	subl	$8, %esp		CC (%esp) = primary fd, 4(%esp) = secondary fd
	movl	$-1, 4(%esp)
.next_action:
	movl	(%ebp), %edx		CC edx = action number
	andl	$-17, %edx
	cmpl	$0, %edx		CC open file?
	je	.open_file
	cmpl	$3, %edx		CC jump?
	je	.rest_of_exec
	cmpl	$4, %edx		CC anonymous mmap?
	je	.do_mmap_anon
.do_mmap:
	subl	$24, %esp
	movl	$90, %eax		CC SYS_old_mmap
	movl	%esp, %ebx
	movl	4(%ebp), %ecx		CC address
	movl	%ecx, (%esp)
	movl	16(%ebp), %ecx		CC length
	movl	%ecx, 4(%esp)
	movl	12(%ebp), %ecx		CC protection
	movl	%ecx, 8(%esp)
	movl	20(%ebp), %ecx		CC flags
	movl	%ecx, 12(%esp)
	testl	$16, (%ebp)		CC primary?
	movl	28(%esp), %ecx
	cmovzl	24(%esp), %ecx
	movl	%ecx, 16(%esp)		CC fd
	movl	8(%ebp), %ecx		CC offset
	movl	%ecx, 20(%esp)
.do_mmap_1:
	int	$0x80
	addl	$24, %esp		CC restore esp
	cmpl	$-1, %eax		CC mmap failed?
	je	.perror
	movl	24(%ebp), %ecx		CC clear
	testl	%ecx, %ecx
	jz	.continue
	movl	4(%ebp), %esi		CC start of mapping
	addl	16(%ebp), %esi		CC end of mapping
	subl	%ecx, %esi		CC start of clear area
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
	movl	$90, %eax		CC SYS_old_mmap
	movl	%esp, %ebx
	movl	4(%ebp), %ecx		CC address
	movl	%ecx, (%esp)
	movl	16(%ebp), %ecx		CC length
	movl	%ecx, 4(%esp)
	movl	12(%ebp), %ecx		CC protection
	movl	%ecx, 8(%esp)
	movl	20(%ebp), %ecx		CC flags
	movl	%ecx, 12(%esp)
	movl	$-1, 16(%esp)		CC fd
	movl	8(%ebp), %ecx		CC offset
	movl	%ecx, 20(%esp)
	jmp	.do_mmap_1
.open_file:
	movl	$5, %eax		CC SYS_open
	leal	4(%ebp), %ebx		CC ebx = %esp + 8
	pushl	%ebx
	xorl	%ecx, %ecx		CC flags = O_RDONLY
	xorl	%edx, %edx		CC mode = 0
	int	$0x80
	cmpl	$-1, %eax		CC open failed?
	jle	.perror
	movl	%ebp, %esi		CC (esi) = original action number
	popl	%ebp			CC ebp = start of string
	movl	%ebp, %ecx		CC char past separator
	decl	%ebp
.nextc:
	incl	%ebp
	movb	(%ebp), %dl		CC dl = *ebp
	cmpb	$47, %dl		CC dl == '\?'?
	jne	.nextc1
	leal	1(%ebp), %ecx		CC ecx = char past separator
.nextc1:
	cmpb	$0, %dl			CC dl == 0?
	jne	.nextc
	addl	$4, %ebp		CC adjust past ebp prior to rounding
	andl	$-4, %ebp		CC round ebp up to the next long
	testl	$16, (%esi)		CC original action number & 16?
	jz	.primary
	movl	%eax, 4(%esp)		CC secondary fd = eax
	jmp	.next_action
.primary:
	pushl	%ebp
	xorl	%esi, %esi		CC arg3
	movl	%eax, 4(%esp)		CC primary fd = eax
	xorl	%edx, %edx		CC arg2
	movl	$15, %ebx		CC PR_SET_NAME, arg1 = ecx
	xorl	%edi, %edi		CC arg4
	movl	$172, %eax		CC SYS_prctl
	xorl	%ebp, %ebp		CC arg5
	int	$0x80			CC syscall
	popl	%ebp
	jmp	.next_action
.perror:
	movl	%eax, %ebx
	negl	%ebx
	movl	$1, %eax
	int	$0x80
.rest_of_exec:
	movl	8(%esp), %ecx		CC ecx = original stack pointer
	movl	(%ecx), %esi		CC esi = argc
	leal	8(%ecx, %esi, 4), %ecx	CC ecx = start of environ
.skip_environ:
	movl	(%ecx), %esi		CC envp[N]
	addl	$4, %ecx
	testl	%esi, %esi		CC envp[n] ?
	jnz	.skip_environ		CC otherwise, esi is now at the start of auxv
.one_auxv:
	movl	(%ecx), %esi		CC auxv type
	leal	8(%ecx), %ecx		CC skip to next auxv
	testl	%esi, %esi		CC is 0?
	jz	.cleanup
	cmpl	$3, %esi		CC is AT_PHDR
	je	.replace_phdr
	cmpl	$4, %esi		CC is AT_PHENT?
	je	.replace_phent
	cmpl	$5, %esi		CC is AT_PHNUM?
	je	.replace_phnum
	cmpl	$9, %esi		CC is AT_ENTRY?
	je	.replace_entry
	cmpl	$7, %esi		CC is AT_BASE
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
	movl	$6, %eax		CC SYS_close
	cmpl	$-1, 4(%esp)		CC see if interpreter fd is set
	je	.cleanup_1
	movl	4(%esp), %ebx
	int 	$0x80
	movl	$6, %eax		CC SYS_close
.cleanup_1:
	movl	(%esp), %ebx
	int	$0x80
.enter:
	pushl	$0
	popfl				CC restore floating point state
	movl	8(%esp), %esp		CC restore initial stack pointer
	xorl	%edx, %edx		CC clear rtld_fini
	jmpl	*4(%ebp)		CC entry

timespec:
	.long	10
	.long	10
