// Copyright (C) 2023-2026 Free Software Foundation, Inc.
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
	// mov	x8, 101			// SYS_nanosleep
	// adr	x0, timespec		// req
	// mov	x1, #0			// rem
	// svc	#0			// syscall
	mov	x20, sp			// x20 = sp
	ldr	x10, [x20]		// x10 = original SP
	add	x20, x20, #16		// x20 = start of load area
	mov	x28, #-1		// x28 = secondary fd
next_action:
	ldr	x11, [x20]		// action number
	and	x12, x11, #-17		// actual action number
	cbz	x12, open_file		// open file?
	cmp	x12, #3			// jump?
	beq	rest_of_exec
	cmp	x12, #4			// anonymous mmap?
	beq	do_mmap_anon
do_mmap:
	ldr	x0, [x20, 8]		// vm_address
	ldr	x1, [x20, 32]		// length
	ldr	x2, [x20, 24]		// protection
	ldr	x3, [x20, 40]		// flags
	tst	x11, #16		// primary fd?
	mov	x4, x29			// primary fd
	beq	do_mmap_1
	mov	x4, x28			// secondary fd
do_mmap_1:
	mov	x8, #222		// SYS_mmap
	ldr	x5, [x20, 16]		// file_offset
	svc	#0			// syscall
	ldr	x9, [x20, 8]		// length
	cmp	x0, x9			// mmap result
	bne	perror			// print error
	ldr	x3, [x20, 48]		// clear
	add	x1, x1, x0		// x1 = vm_address + end
	sub	x3, x1, x3		// x3 = x1 - clear
	mov	x0, #0			// x0 = 0
fill64:
	sub	x2, x1, x3		// x2 = x1 - x3
	cmp	x2, #63			// x2 >= 64?
	ble	fillb			// start filling bytes
	stp	x0, x0, [x3]		// x3[0] = 0, x3[1] = 0
	stp	x0, x0, [x3, 16]	// x3[2] = 0, x3[3] = 0
	stp	x0, x0, [x3, 32]	// x3[4] = 0, x3[5] = 0
	stp	x0, x0, [x3, 48]	// x3[6] = 0, x3[7] = 0
	add	x3, x3, #64		// x3 += 8
	b	fill64
fillb:
	cmp	x1, x3			// x1 == x3?
	beq	continue		// done
	strb	w0, [x3], #1		// ((char *) x3)++ = 0
	b	fillb
continue:
	add	x20, x20, #56		// next action
	b	next_action
do_mmap_anon:
	ldr	x0, [x20, 8]		// vm_address
	ldr	x1, [x20, 32]		// length
	ldr	x2, [x20, 24]		// protection
	ldr	x3, [x20, 40]		// flags
	mov	x4, #-1			// fd
	b	do_mmap_1
open_file:
	mov	x8, #56			// SYS_openat
	mov	x0, #-100		// AT_FDCWD
	add	x1, x20, #8		// file name
	mov	x2, #0			// O_RDONLY
	mov	x3, #0			// mode
	svc	#0			// syscall
	cmp	x0, #-1			// rc < 0?
	ble	perror
	mov	x19, x1			// x19 == x1
nextc:
	ldrb	w2, [x1], #1		// b = *x1++
	cmp	w2, #47			// dir separator?
	bne	nextc1			// not dir separator
	mov	x19, x1			// x19 = char past separator
nextc1:
	cbnz	w2, nextc		// b?
	add	x1, x1, #7		// round up x1
	and	x20, x1, #-8		// mask for round, set x20
	tst	x11, #16		// primary fd?
	bne	secondary		// secondary fd
	mov	x29, x0			// primary fd
	mov	x8, #167		// SYS_prctl
	mov	x0, #15			// PR_SET_NAME
	mov	x1, x19			// basename
	mov	x2, #0			// arg2
	mov	x3, #0			// arg3
	mov	x4, #0			// arg4
	mov	x5, #0			// arg5
	svc	#0			// syscall
	b	next_action		// next action
secondary:
	mov	x28, x0			// secondary fd
	b	next_action		// next action.
perror:
	mov	x8, #93			// SYS_exit
	mvn	x0, x0			// x1 = ~x0
	add	x0, x0, 1		// x1 += 1
	svc	#0			// exit
rest_of_exec:
	mov	x7, x20			// x7 = x20
	mov	x8, x10			// x8 = x10
	ldr	x9, [x8], #16		// (void *) x8 += 2
	lsl	x9, x9, #3		// argc * 8
	add	x8, x8, x9		// now past argv
skip_environ:
	ldr	x9, [x8], #8		// x9 = *envp++
	cbnz	x9, skip_environ	// x9?
	// Skip the auxiliary vector.
1:	ldp	x11, x12, [x8], #16	// a_type, a_un.a_val
	cbnz	x11, 1b			// a_type != NULL
	// Prepare sufficient space at x20 for the file name string.
	// Load the aforesaid string, and its length.
	ldr	x6, [x7, 56]		// string length
	add	x6, x6, 1
	add	x5, x7, 64		// string pointer
	sub	x4, x10, x8		// number of elements to copy
	sub	x7, x8, x6		// AT_EXECFN location
	and	x7, x7, -8		// align value
	add	x4, x7, x4		// destination argc
	and	x4, x4, -16		// align destination argc
	// Load values that must be preserved into registers x14-x19.
	// x14 = cmd->entry
	// x15 = cmd->at_entry
	// x16 = cmd->at_phent
	// x17 = cmd->at_phnum
	// x18 = cmd->at_phdr
	// x19 = cmd->at_base
	ldp	x14, x15, [x20, 8]
	ldp	x16, x17, [x20, 24]
	ldp	x18, x19, [x20, 40]
	// Move the string to a safe location, if necessary.
	sub	x3, x4, x5		// distance from dest to string
	cmp	x3, x6			// distance > length
	bge	copy_env_and_args 	// not necessary
	mov	x2, x5			// src
	sub	x5, x4, x6		// backup string
	mov	x1, x5			// dst
	add	x9, x2, x6		// src end
	cmp	x2, x9
	bcs	copy_env_and_args
1:	ldrb	w3, [x2], #1
	strb	w3, [x1], #1
	cmp	x2, x9
	blo	1b
copy_env_and_args:
	// Copy argc and the environment array.
	mov	x8, x10
	mov	x10, x4
1:	ldr	x9, [x8], #8		//  envp
	str	x9, [x4], #8
	cbnz	x9, 1b
1:	ldr	x9, [x8], #8		// environ
	str	x9, [x4], #8
	cbnz	x9, 1b
copy_auxv:
	ldp	x11, x12, [x8], #16	// a_type, a_un.a_val
	stp	x11, x12, [x4], #16	// write value
	cbz	x11, cleanup		// AT_NULL
	cmp	x11, #3			// AT_PHDR
	csel	x12, x18, x12, eq
	cmp	x11, #4			// AT_PHENT
	csel	x12, x16, x12, eq
	cmp	x11, #5			// AT_PHNUM
	csel	x12, x17, x12, eq
	cmp	x11, #9			// AT_ENTRY
	csel	x12, x15, x12, eq
	cmp	x11, #7			// AT_BASE
	csel	x12, x19, x12, eq
	cmp	x11, #31		// AT_EXECFN
	csel	x12, x7, x12, eq
	str	x12, [x4, -8]		// replace value
	b	copy_auxv
cleanup:
	// Copy the filename.
	add	x9, x5, x6		// end
	cmp	x5, x9
	bcs	2f
1:	ldrb	w3, [x5], #1
	strb	w3, [x7], #1
	cmp	x5, x9
	blo	1b
	// Close file descriptors.
2:	cmp	x28, #-1		// is secondary fd set?
	beq	cleanup1		// not set
	mov	x8, #57			// SYS_close
	mov	x0, x28			// secondary fd
	svc	#0			// syscall
cleanup1:
	mov	x8, #57			// SYS_close
	mov	x0, x29			// primary fd
	svc	#0			// syscall
enter:
	mov	sp, x10			// restore original SP
	mov	x0, #0			// clear rtld_fini
	br	x14

// timespec:
// 	.quad 10
// 	.quad 10

// Local Variables:
// asm-comment-char: ?/
// End:
