// Copyright (C) 2023 Free Software Foundation, Inc.
//
// This file is part of GNU Emacs.
//
// GNU Emacs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License,
// or (at your option) any later version.
//
// GNU Emacs is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

// Notice that aarch64 requires that sp be aligned to 16 bytes while
// accessing memory from sp, so x20 is used to chase down the load
// area.

	.section .text
	.global _start
_start:
	//mov	x8, 101			// SYS_nanosleep
	//adr	x0, timespec		// req
	//mov	x1, #0			// rem
	//svc	#0			// syscall
	mov	x20, sp			// x20 = sp
	ldr	x10, [x20]		// x10 = original SP
	add	x20, x20, #16		// x20 = start of load area
	mov	x28, #-1		// x28 = secondary fd
.next_action:
	ldr	x11, [x20]		// action number
	and	x12, x11, #-17		// actual action number
	cbz	x12, .open_file		// open file?
	cmp	x12, #3			// jump?
	beq	.rest_of_exec
	cmp	x12, #4			// anonymous mmap?
	beq	.do_mmap_anon
.do_mmap:
	ldr	x0, [x20, 8]		// vm_address
	ldr	x1, [x20, 32]		// length
	ldr	x2, [x20, 24]		// protection
	ldr	x3, [x20, 40]		// flags
	tst	x11, #16		// primary fd?
	mov	x4, x29			// primary fd
	beq	.do_mmap_1
	mov	x4, x28			// secondary fd
.do_mmap_1:
	mov	x8, #222		// SYS_mmap
	ldr	x5, [x20, 16]		// file_offset
	svc	#0			// syscall
	ldr	x9, [x20, 8]		// length
	cmp	x0, x9			// mmap result
	bne	.perror			// print error
	ldr	x3, [x20, 48]		// clear
	add	x1, x1, x0		// x1 = vm_address + end
	sub	x3, x1, x3		// x3 = x1 - clear
	mov	x0, #0			// x0 = 0
.fill64:
	sub	x2, x1, x3		// x2 = x1 - x3
	cmp	x2, #63			// x2 >= 64?
	ble	.fillb			// start filling bytes
	stp	x0, x0, [x3]		// x3[0] = 0, x3[1] = 0
	stp	x0, x0, [x3, 16]	// x3[2] = 0, x3[3] = 0
	stp	x0, x0, [x3, 32]	// x3[4] = 0, x3[5] = 0
	stp	x0, x0, [x3, 48]	// x3[6] = 0, x3[7] = 0
	add	x3, x3, #64		// x3 += 8
	b	.fill64
.fillb:
	cmp	x1, x3			// x1 == x3?
	beq	.continue		// done
	strb	w0, [x3], #1		// ((char *) x3)++ = 0
	b	.fillb
.continue:
	add	x20, x20, #56		// next action
	b	.next_action
.do_mmap_anon:
	ldr	x0, [x20, 8]		// vm_address
	ldr	x1, [x20, 32]		// length
	ldr	x2, [x20, 24]		// protection
	ldr	x3, [x20, 40]		// flags
	mov	x4, #-1			// fd
	b	.do_mmap_1
.open_file:
	mov	x8, #56			// SYS_openat
	mov	x0, #-100		// AT_FDCWD
	add	x1, x20, #8		// file name
	mov	x2, #0			// O_RDONLY
	mov	x3, #0			// mode
	svc	#0			// syscall
	cmp	x0, #-1			// rc < 0?
	ble	.perror
.nextc:
	ldrb	w2, [x1], #1		// b = *x1++
	cbnz	w2, .nextc		// b?
	add	x1, x1, #7		// round up x1
	and	x20, x1, #-8		// mask for round, set x20
	tst	x11, #16		// primary fd?
	bne	.secondary		// secondary fd
	mov	x29, x0			// primary fd
	b	.next_action		// next action
.secondary:
	mov	x28, x0			// secondary fd
	b	.next_action		// next action.
.perror:
	mov	x8, #93			// SYS_exit
	mvn	x0, x0			// x1 = ~x0
	add	x0, x0, 1		// x1 += 1
	svc	#0			// exit
.rest_of_exec:
	mov	x7, x20			// x7 = x20
	mov	x20, x10		// x20 = x10
	ldr	x9, [x20]		// argc
	add	x9, x9, #2		// x9 += 2
	lsl	x9, x9, #3		// argc * 8
	add	x20, x20, x9		// now past argv
.skipenv:
	ldr	x9, [x20], #8		// x9 = *envp++
	cbnz	x9, .skipenv		// x9?
.one_auxv:
	ldr	x9, [x20], #16		// x9 = *sp, sp += 2
	cbz	x9, .cleanup		// !x9?
	cmp	x9, #3			// is AT_PHDR?
	beq	.replace_phdr		// replace
	cmp	x9, #4			// is AT_PHENT?
	beq	.replace_phent		// replace
	cmp	x9, #5			// is AT_PHNUM?
	beq	.replace_phnum		// replace
	cmp	x9, #9			// is AT_ENTRY?
	beq	.replace_entry		// replace
	cmp	x9, #7			// is AT_BASE?
	beq	.replace_base		// replace
	b	.one_auxv		// next auxv
.replace_phdr:
	ldr	x9, [x7, 40]		// at_phdr
	str	x9, [x20, -8]		// store value
	b	.one_auxv
.replace_phent:
	ldr	x9, [x7, 24]		// at_phent
	str	x9, [x20, -8]		// store value
	b	.one_auxv
.replace_phnum:
	ldr	x9, [x7, 32]		// at_phnum
	str	x9, [x20, -8]		// store value
	b	.one_auxv
.replace_entry:
	ldr	x9, [x7, 16]		// at_entry
	str	x9, [x20, -8]		// store value
	b	.one_auxv
.replace_base:
	ldr	x9, [x7, 48]		// at_base
	str	x9, [x20, -8]		// store value
	b	.one_auxv
.cleanup:
	cmp	x28, #-1		// is secondary fd set?
	bne	.cleanup1		// not set
	mov	x8, #57			// SYS_close
	mov	x0, x28			// secondary fd
	svc	#0			// syscall
.cleanup1:
	mov	x8, #57			// SYS_close
	mov	x0, x29			// primary fd
	svc	#0			// syscall
.enter:
	mov	sp, x10			// restore original SP
	mov	x0, #0			// clear rtld_fini
	ldr	x1, [x7, 8]		// branch to code
	br	x1

timespec:
	.quad 10
	.quad 10
