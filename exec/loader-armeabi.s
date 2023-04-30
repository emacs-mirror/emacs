@ Copyright (C) 2023 Free Software Foundation, Inc.
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
	@mov	r7, #162		@ SYS_nanosleep
	@adr	r0, timespec		@ req
	@mov	r1, #0			@ rem
	@swi	#0			@ syscall
	mov	r8, sp			@ r8 = sp
	ldr	r9, [r8], #8		@ r9 = original sp, r8 += 8
	mov	r14, #-1		@ r14 = secondary fd
.next_action:
	ldr	r11, [r8]		@ r11 = action number
	and	r12, r11, #-17		@ actual action number
	cmp	r12, #0			@ open file?
	beq	.open_file		@ open file.
	cmp	r12, #3			@ jump?
	beq	.rest_of_exec		@ jump to code.
	cmp	r12, #4			@ anonymous mmap?
	beq	.do_mmap_anon		@ anonymous mmap.
.do_mmap:
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
	beq	.do_mmap_1
	mov	r4, r14			@ secondary fd
.do_mmap_1:
	mov	r7, #192		@ SYS_mmap2
	swi	#0			@ syscall
	ldr	r2, [r8, #4]		@ vm_address
	cmp	r2, r0			@ rc == vm_address?
	bne	.perror
	add	r0, r1, r2		@ r0 = length + vm_address
	sub	r3, r0, r12		@ r3 = r0 - clear
	mov	r1, #0			@ r1 = 0
.align:
	cmp	r0, r3			@ r0 == r3?
	beq	.continue		@ continue
	tst	r3, #3			@ r3 & 3?
	bne	.fill32			@ fill aligned
	strb	r1, [r3], #1		@ fill byte
	b	.align			@ align again
.fill32:
	sub	r2, r0, r3		@ r2 = r0 - r3
	cmp	r2, #31			@ r2 >= 32?
	ble	.fillb			@ start filling bytes
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	str	r1, [r3], #4		@ *r3++ = 0
	b	.fill32
.fillb:
	cmp	r0, r3			@ r0 == r3
	beq	.continue		@ done
	strb	r1, [r3], #1		@ ((char *) r3)++ = 0
	b	.fillb
.continue:
	add	r8, r8, #28		@ next action
	b	.next_action
.do_mmap_anon:
	add	r6, r8, #4		@ r6 = r8 + 4
	ldm	r6!, {r0, r5}		@ vm_address, file_offset
	ldm	r6!, {r1, r2}		@ protection, length
	mov	r3, r1			@ swap
	lsr	r5, #12			@ divide file offset by page size
	mov	r1, r2			@ swap
	mov	r2, r3			@ swap
	ldm	r6!, {r3, r12}		@ flags, clear
	mov	r4, #-1			@ fd
	b	.do_mmap_1
.open_file:
	mov	r7, #5			@ SYS_open
	add	r0, r8, #4		@ file name
	mov	r1, #0			@ O_RDONLY
	mov	r2, #0			@ mode
	swi	#0			@ syscall
	cmp	r0, #-1			@ r0 <= -1?
	ble	.perror
	add	r8, r8, #4		@ r8 = start of string
.nextc:
	ldrb	r1, [r8], #1		@ b = *r0++
	cmp	r1, #0			@ b?
	bne	.nextc			@ next character
	add	r8, r8, #3		@ round up r8
	and	r8, r8, #-4		@ mask for round, set r8
	tst	r11, #16		@ primary fd?
	bne	.secondary		@ secondary fd
	mov	r10, r0			@ primary fd
	b	.next_action		@ next action
.secondary:
	mov	r14, r0			@ secondary fd
	b	.next_action		@ next action
.perror:
	mov	r7, #1			@ SYS_exit
	mvn	r0, r0			@ r0 = ~r0
	add	r0, r0, #1		@ r0 += 1
	swi	#0
.rest_of_exec:
	mov	r7, r9			@ r7 = original SP
	ldr	r6, [r7]		@ argc
	add	r6, r6, #2		@ argc + 2
	lsl	r6, r6, #2		@ argc *= 4
	add	r7, r7, r6		@ now past argv
.skipenv:
	ldr	r6, [r7], #4		@ r6 = *r7++
	cmp	r6, #0			@ r6?
	bne	.skipenv		@ r6?
.one_auxv:
	ldr	r6, [r7], #8		@ r6 = *r7, r7 += 2
	cmp	r6, #0			@ !r6?
	beq	.cleanup		@ r6?
	cmp	r6, #3			@ is AT_PHDR?
	beq	.replace_phdr		@ replace
	cmp	r6, #4			@ is AT_PHENT?
	beq	.replace_phent		@ replace
	cmp	r6, #5			@ is AT_PHNUM?
	beq	.replace_phnum		@ replace
	cmp	r6, #9			@ is AT_ENTRY?
	beq	.replace_entry		@ replace
	cmp	r6, #7			@ is AT_BASE?
	beq	.replace_base		@ replace
	b	.one_auxv		@ next auxv
.replace_phdr:
	ldr	r6, [r8, #20]		@ at_phdr
	str	r6, [r7, #-4]		@ store value
	b	.one_auxv
.replace_phent:
	ldr	r6, [r8, #12]		@ at_phent
	str	r6, [r7, #-4]		@ store value
	b	.one_auxv
.replace_phnum:
	ldr	r6, [r8, #16]		@ at_phnum
	str	r6, [r7, #-4]		@ store value
	b	.one_auxv
.replace_entry:
	ldr	r6, [r8, #8]		@ at_entry
	str	r6, [r7, #-4]		@ store value
	b	.one_auxv
.replace_base:
	ldr	r6, [r8, #24]		@ at_base
	str	r6, [r7, #-4]		@ store value
	b	.one_auxv
.cleanup:
	cmp	r14, #-1		@ secondary fd set?
	bne	.cleanup1		@ not set
	mov	r7, #6			@ SYS_close
	mov	r0, r14			@ secondary fd
	swi	#0			@ syscall
.cleanup1:
	mov	r7, #6			@ SYS_close
	mov	r0, r10			@ primary fd
	swi	#0			@ syscall
.enter:
	mov	sp, r9			@ restore original SP
	mov	r0, #0			@ clear rtld_fini
	ldr	r1, [r8, #4]		@ branch to code
	bx	r1

timespec:
	.long 10
	.long 10

@ Local Variables:
@ asm-comment-char: 64
@ End:
