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

/* Sorry!  This program is a hopeless shambles in consequence of
   being hastily written in under twenty minutes with minimal testing.  */

	.section .text
	.global _start
_start:
	## movl	$162, %eax		# SYS_nanosleep
	## leal	timespec, %ebx
	## xorl	%ecx, %ecx
	## int	$0x80
	leal	8(%esp), %ebp		# ebp = start of load area
	subl	$8, %esp		# (%esp) = primary fd, 4(%esp) = secondary fd
	movl	$-1, 4(%esp)
next_action:
	movl	(%ebp), %edx		# edx = action number
	andl	$-17, %edx
	cmpl	$0, %edx		# open file?
	je	open_file
	cmpl	$3, %edx		# jump?
	je	rest_of_exec
	cmpl	$4, %edx		# anonymous mmap?
	je	do_mmap_anon
do_mmap:
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
do_mmap_1:
	int	$0x80
	addl	$24, %esp		# restore esp
	cmpl	$-1, %eax		# mmap failed?
	je	perror
	movl	24(%ebp), %ecx		# clear
	testl	%ecx, %ecx
	jz	continue
	movl	4(%ebp), %esi		# start of mapping
	addl	16(%ebp), %esi		# end of mapping
	subl	%ecx, %esi		# start of clear area
again:
	testl	%ecx, %ecx
	jz	continue
	subl	$1, %ecx
	movb	$0, (%esi, %ecx, 1)
	jmp	again
continue:
	leal	28(%ebp), %ebp
	jmp	next_action
do_mmap_anon:
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
	jmp	do_mmap_1
open_file:
	movl	$5, %eax		# SYS_open
	leal	4(%ebp), %ebx		# ebx = %esp + 8
	pushl	%ebx
	xorl	%ecx, %ecx		# flags = O_RDONLY
	xorl	%edx, %edx		# mode = 0
	int	$0x80
	cmpl	$-1, %eax		# open failed?
	jle	perror
	movl	%ebp, %esi		# (esi) = original action number
	popl	%ebp			# ebp = start of string
	movl	%ebp, %ecx		# char past separator
	decl	%ebp
nextc:
	incl	%ebp
	movb	(%ebp), %dl		# dl = *ebp
	cmpb	$47, %dl		# dl == '\?'?
	jne	nextc1
	leal	1(%ebp), %ecx		# ecx = char past separator
nextc1:
	cmpb	$0, %dl			# dl == 0?
	jne	nextc
	addl	$4, %ebp		# adjust past ebp prior to rounding
	andl	$-4, %ebp		# round ebp up to the next long
	testl	$16, (%esi)		# original action number & 16?
	jz	primary
	movl	%eax, 4(%esp)		# secondary fd = eax
	jmp	next_action
primary:
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
	jmp	next_action
perror:
	movl	%eax, %ebx
	negl	%ebx
	movl	$1, %eax
	int	$0x80
rest_of_exec:
	movl	8(%esp), %ecx		# ecx = original stack pointer
	movl	(%ecx), %esi		# esi = argc
	leal	8(%ecx, %esi, 4), %ecx	# ecx = start of environ
	movl	(%esp), %eax		# %eax = primary fd
	movl	4(%esp), %edi		# %edi = secondary fd
skip_environ:
	movl	(%ecx), %esi		# envp[N]
	addl	$4, %ecx
	testl	%esi, %esi		# envp[n] ?
	jnz	skip_environ		# otherwise, ecx is now at the end of auxv
1:	testl	$-1, (%ecx)		# auxv type
	leal	8(%ecx), %ecx		# skip to next auxv
	jnz	1b			# otherwise copy auxv
	movl	%ecx, %edx		# end of auxv
	/* Prepare sufficient space for the new executable name at the
	   start of the auxiliary vector.  */
1:	leal	32(%ebp), %esi		# file name
	/* 28(%ebp) = file name length.  */
	subl	28(%ebp), %ecx		# destination of file name
	decl	%ecx
	/* This is still 16 bytes on i386--see arch_align_stack:
	   https://android.googlesource.com/kernel/goldfish/+/refs/heads
	   /android-goldfish-3.10/arch/x86/kernel/process.c#446.  */
	andl	$-16, %ecx		# align stack
	/* Prepare to store the auxiliary, environment, and argument
	   vectors.  */
	subl	8(%esp), %edx		# end of auxv to start of stack
	negl	%edx
	andl	$-16, %edx		# align value
	movl	%ecx, (%ebp)		# temporarily save ecx
	addl	%edx, %ecx		# %ecx = new position of argc
	/* Allocate a temporary stack away from any crucial data in which
	   to store parameters and temporaries.  */
	cmpl	%ecx, %ebp		# select position of temporary stack
	movl	%ecx, %ebx		# ebx = temporary stack
	jge	1f			# %ebx = MIN (%ecx, %edx)
	movl	%ebp, %ebx		# ebx = temporary stack
1:	movl	(%ebp), %edx		# edx = destination of file name
	movl	%edx, -4(%ebx)		# -4(%ebx) = destination of file name
	movl	28(%ebp), %edx		# file name length
	movl	%edx, -8(%ebx)		# -8(%ebx) = file name length
	movl	%ecx, -12(%ebx)		# -12(%ebx) = new position of argc
	movl	%esi, -16(%ebx)		# -16(%ebx) = file name
	movl	8(%esp), %edx		# %edx = initial stack pointer
	leal	-16(%ebx), %esp		# switch to temporary stack
	/* Push parameters of `struct exec_jump_command'.  */
	push	%edx			# initial stack pointer -20(%ebx)
	push	4(%ebp)			# entry -24(%ebx)
	push	8(%ebp)			# at_entry -28(%ebx)
	push	12(%ebp)		# at_phent -32(%ebx)
	push	16(%ebp)		# at_phnum -36(%ebx)
	push	20(%ebp)		# at_phdr -40(%ebx)
	push	24(%ebp)		# at_base -44(%ebx)
	/* Push primary and secondary fds.  */
	push	%eax			# primary fd -48(%ebx)
	push	%edi			# secondary fd -52(%ebx)
	/* Swap %ebp with %ebx.  */
	push	%ebp
	push	%ebx
	pop	%ebp
	pop	%ebx			# ebx is the exec_jump_command
	/* Save the string lest it should be overwritten while
	   the environment is moved.  */
	movl	-8(%ebp), %ecx
	subl	$4, %esp		# -56(%ebp)
	subl	%ecx, %esp
	leal	-1(%esp), %edi
	movl	%edi, -56(%ebp)		# copy of string
	incl	%ecx
	movl	%edi, %esp
	cld
  rep	movsb				# complete copy
	andl	$-4, %esp		# align stack
	movl	-12(%ebp), %ecx
	/* Begin moving the argument vectors and environment from
	   the original SP to the adjusted one.  */
1:	movl	(%edx), %eax		# argc and values
	movl	%eax, (%ecx)
	leal	4(%ecx), %ecx
	leal	4(%edx), %edx
	testl	%eax, %eax
	jnz	1b
1:	movl	(%edx), %eax		# envp
	movl	%eax, (%ecx)
	leal	4(%ecx), %ecx
	leal	4(%edx), %edx
	testl	%eax, %eax
	jnz	1b
copy_auxv:
	movl	(%edx), %eax		# a_type
	movl	4(%edx), %esi		# a_un.a_val
	testl	%eax, %eax
	leal	8(%edx), %edx
	movl	%eax, (%ecx)		# copy auxv type
	leal	8(%ecx), %ecx
	jz	cleanup			# AT_NULL
	cmpl	$3, %eax		# AT_PHDR
	jz	1f
	cmpl	$4, %eax		# AT_PHENT
	jz	2f
	cmpl	$5, %eax		# AT_PHNUM
	jz	3f
	cmpl	$9, %eax		# AT_ENTRY
	jz	4f
	cmpl	$7, %eax		# AT_BASE
	jz	5f
	cmpl	$31, %eax		# AT_EXECFN
	jz	6f
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
1:	movl	-40(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
2:	movl	-32(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
3:	movl	-36(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
4:	movl	-28(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
5:	movl	-44(%ebp), %esi
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
6:	movl	-4(%ebp), %esi		# Note: the filename is yet to be copied.
	movl	%esi, -4(%ecx)
	jmp	copy_auxv
cleanup:
	movl	$0, -4(%ecx)		# AT_NULL value
	/* Copy data for AT_EXECFN to the destination address.  */
	movl	-4(%ebp), %edi
	movl	-56(%ebp), %esi
	movl	-8(%ebp), %ecx
	incl	%ecx
  rep	movsb
	movl	$6, %eax		# SYS_close
	cmpl	$-1, -52(%ebp)		# see if interpreter fd is set
	je	cleanup_1
	movl	-52(%ebp), %ebx
	int 	$0x80
	movl	$6, %eax		# SYS_close
cleanup_1:
	movl	-48(%ebp), %ebx
	int	$0x80
enter:
	pushl	$0
	popfl				# restore floating point state
	movl	-12(%ebp), %esp		# restore initial stack pointer
	xorl	%edx, %edx		# clear rtld_fini
	jmpl	*-24(%ebp)		# entry
## timespec:
## 	.long	10
## 	.long	10

# Local Variables:
# asm-comment-char: ?#
# End:
