@ Copyright (C) 2023-2026 Free Software Foundation, Inc.
@
@ This file is part of GNU Emacs.
@
@ GNU Emacs is free software: you can redistribute it and/or modify
@ it under the terms of the GNU General Public License as published
@ by the Free Software Foundation, either version 3 of the License,
@ or (at your option) any later version.
@
@ GNU Emacs is distributed in the hope that it will be useful, but
@ WITHOUT ANY WARRANTY; without even the implied warranty of
@ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
@ General Public License for more details.
@
@ You should have received a copy of the GNU General Public License
@ along with GNU Emacs.  If not, see <https:@www.gnu.org/licenses/>.

	.section .text
	.global _start
_start:
	@@ mov	r7, #162		@ SYS_nanosleep
	@@ adr	r0, timespec		@ req
	@@ mov	r1, #0			@ rem
	@@ swi	#0			@ syscall
	mov	r8, sp			@ r8 = sp
	ldr	r9, [r8], #8		@ r9 = original sp, r8 += 8
	mov	r14, #-1		@ r14 = secondary fd
next_action:
	ldr	r11, [r8]		@ r11 = action number
	and	r12, r11, #-17		@ actual action number
	cmp	r12, #0			@ open file?
	beq	open_file		@ open file.
	cmp	r12, #3			@ jump?
	beq	rest_of_exec		@ jump to code.
	cmp	r12, #4			@ anonymous mmap?
	beq	do_mmap_anon		@ anonymous mmap.
do_mmap:
	add	r6, r8, #4		@ r6 = r8 + 4
	ldm	r6!, {r0, r5}		@ vm_address, file_offset
	ldm	r6!, {r1, r2}		@ protection, length
	mov	r3, r1			@ swap
	lsr	r5, #12			@ divide file offset by page size
	mov	r1, r2			@ swap
	mov	r2, r3			@ swap
	ldm	r6!, {r3, r12}		@ flags, clear
	tst	r11, #16		@ primary fd?
	mov	r4, r10			@ primary fd
	beq	do_mmap_1
	mov	r4, r14			@ secondary fd
do_mmap_1:
	mov	r7, #192		@ SYS_mmap2
	swi	#0			@ syscall
	ldr	r2, [r8, #4]		@ vm_address
	cmp	r2, r0			@ rc == vm_address?
	bne	perror
	add	r0, r1, r2		@ r0 = length + vm_address
	sub	r3, r0, r12		@ r3 = r0 - clear
	mov	r1, #0			@ r1 = 0
align:
	cmp	r0, r3			@ r0 == r3?
	beq	continue		@ continue
	tst	r3, #3			@ r3 & 3?
	bne	fill32			@ fill aligned
	strb	r1, [r3], #1		@ fill byte
	b	align			@ align again
fill32:
	sub	r2, r0, r3		@ r2 = r0 - r3
	cmp	r2, #31			@ r2 >= 32?
	ble	fillb			@ start filling bytes
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	b	fill32
fillb:
	cmp	r0, r3			@ r0 == r3
	beq	continue		@ done
	strb	r1, [r3], #1		@ ((char *) r3)++ = 0
	b	fillb
continue:
	add	r8, r8, #28		@ next action
	b	next_action
do_mmap_anon:
	add	r6, r8, #4		@ r6 = r8 + 4
	ldm	r6!, {r0, r5}		@ vm_address, file_offset
	ldm	r6!, {r1, r2}		@ protection, length
	mov	r3, r1			@ swap
	lsr	r5, #12			@ divide file offset by page size
	mov	r1, r2			@ swap
	mov	r2, r3			@ swap
	ldm	r6!, {r3, r12}		@ flags, clear
	mov	r4, #-1			@ fd
	b	do_mmap_1
open_file:
	mov	r7, #5			@ SYS_open
	add	r0, r8, #4		@ file name
	mov	r1, #0			@ O_RDONLY
	mov	r2, #0			@ mode
	swi	#0			@ syscall
	cmp	r0, #-1			@ r0 <= -1?
	ble	perror
	add	r8, r8, #4		@ r8 = start of string
	mov	r1, r8			@ r1 = r8
nextc:
	ldrb	r2, [r8], #1		@ b = *r0++
	cmp	r2, #47			@ dir separator?
	bne	nextc1			@ not dir separator
	mov	r1, r8			@ r1 = char past separator
nextc1:
	cmp	r2, #0			@ b?
	bne	nextc			@ next character
	add	r8, r8, #3		@ round up r8
	and	r8, r8, #-4		@ mask for round, set r8
	tst	r11, #16		@ primary fd?
	bne	secondary		@ secondary fd
	mov	r10, r0			@ primary fd
	mov	r7, #172		@ SYS_prctl
	mov	r0, #15			@ PR_SET_NAME, r1 = name
	mov	r2, #0			@ arg2
	mov	r3, #0			@ arg3
	mov	r4, #0			@ arg4
	mov	r5, #0			@ arg5
	swi	#0			@ syscall
	b	next_action		@ next action
secondary:
	mov	r14, r0			@ secondary fd
	b	next_action		@ next action
perror:
	mov	r7, #1			@ SYS_exit
	mvn	r0, r0			@ r0 = ~r0
	add	r0, r0, #1		@ r0 += 1
	swi	#0
rest_of_exec:				@ r8 points to seven ints + string
	mov	r7, r9			@ r7 = original SP
	ldr	r6, [r7], #8		@ argc & terminator
	lsl	r6, r6, #2		@ argc *= 4
	add	r7, r7, r6		@ now past argv
	ldr	r6, [r8, #28]		@ length of string
	add	r6, r6, #1
skip_environ:
1:	ldr	r1, [r7], #4		@ r1 = *r7++
	tst	r1, r1			@ r1
	bne	1b			@ r1
1:	ldm	r7!, {r0, r1}		@ a_type, a_un.a_val
	tst	r0, r0
	bne	1b			@ a_type -> 1b
	@@ Establish the number of bytes in the argument, environment,
	@@ and auxiliary vectors to be moved.
	sub	r5, r7, r9		@ r5 = bytes in vectors
	@@ Expand r7 with sufficient space for the filename and align
	@@ it.
	sub	r4, r7, r5
	and	r4, r4, #-8		@ r4 = address of AT_EXECFN
	sub	r3, r4, r5		@ r4 - number of bytes in vectors
	and	r3, r3, #-16		@ r3 = position of new argc
	@@ Reserve an area that is guaranteed not to be clobbered into
	@@ which to copy the command and file name.
	mov	r2, r3
	cmp	r2, r8
	blo	1f
	mov	r2, r8
1:	sub	r2, r2, #24		@ space for data
	@@ [r2, #0] = entry
	@@ [r2, #4] = at_entry
	@@ [r2, #8] = at_phent
	@@ [r2, #12] = at_phnum
	@@ [r2, #16] = at_phdr
	@@ [r2, #20] = at_base
	add	r7, r8, #4		@ &cmd->entry
	ldm	r7!, {r0, r1}
	stm	r2!, {r0, r1}
	ldm	r7!, {r0, r1}
	stm	r2!, {r0, r1}
	ldm	r7!, {r0, r1}
	stm	r2!, {r0, r1}
	sub	r2, r2, #24
	sub	r0, r2, r6		@ r0 = copy of AT_EXECFN
	add	r1, r8, #32		@ src
	add	r5, r1, r6		@ src end
	cmp	r1, r5
	bcs	copy_env_and_args
1:	ldrb	r7, [r1], #1
	strb	r7, [r0], #1
	cmp	r1, r5
	blo	1b
copy_env_and_args:
	mov	r5, r3
1:	ldr	r0, [r9], #4		@ argc and arguments
	str	r0, [r5], #4		@ *dst = ...
	tst	r0, r0
	bne	1b
1:	ldr	r0, [r9], #4		@ environment string
	str	r0, [r5], #4		@ *dst = ...
	tst	r0, r0
	bne	1b
copy_auxv:
	ldm	r9!, {r0, r1}		@ a_type, a_un.a_val
	tst	r0, r0			@ AT_NULL
	beq	8f
	cmp	r0, #3			@ AT_PHDR
	beq	2f
	cmp	r0, #4			@ AT_PHENT
	beq	3f
	cmp	r0, #5			@ AT_PHNUM
	beq	4f
	cmp	r0, #9			@ AT_ENTRY
	beq	5f
	cmp	r0, #7			@ AT_BASE
	beq	6f
	cmp	r0, #31			@ AT_EXECFN
	beq	7f
1:	stm	r5!, {r0, r1}
	b	copy_auxv
2:	ldr	r1, [r2, #16]
	b	1b
3:	ldr	r1, [r2, #8]
	b	1b
4:	ldr	r1, [r2, #12]
	b	1b
5:	ldr	r1, [r2, #4]
	b	1b
6:	ldr	r1, [r2, #20]
	b	1b
7:	mov	r1, r4
	b	1b
8:
	stm	r5!, {r0, r1}
cleanup:
	@@ Copy the filename.
	sub	r0, r2, r6		@ src
	add	r1, r0, r6		@ src end
	cmp	r0, r1
	bcs	2f
1:	ldrb	r5, [r0], #1
	strb	r5, [r4], #1		@ *dst++
	cmp	r0, r1
	blo	1b
2:	mov	r9, r3			@ replace original SP
	cmp	r14, #-1		@ secondary fd set?
	beq	cleanup1		@ not set
	mov	r7, #6			@ SYS_close
	mov	r0, r14			@ secondary fd
	swi	#0			@ syscall
cleanup1:
	mov	r7, #6			@ SYS_close
	mov	r0, r10			@ primary fd
	swi	#0			@ syscall
enter:
	mov	sp, r9			@ restore original SP
	mov	r0, #0			@ clear rtld_fini
	ldr	r1, [r2]		@ branch to code
	bx	r1

@@ timespec:
@@ 	.long 10
@@ 	.long 10

@ Local Variables:
@ asm-comment-char: ?@
@ End:
