/* Copyright (C) 1992-1993, 2016-2020 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <sysdeps/unix/sysdep.h>

#define	ENTRY(name)							      \
  .globl _##name;							      \
  .align 2;								      \
  _##name##:

#define	PSEUDO(name, syscall_name, args)				      \
  .text;								      \
  .globl syscall_error;							      \
  ENTRY (name)								      \
    XCHG_##args
    movl $SYS_##syscall_name, %eax;					      \
    int $0x80;								      \
    test %eax, %eax;							      \
    jl syscall_error;							      \
    XCHG_##args

/* Linux takes system call arguments in registers:
   	1: %ebx
	2: %ecx
	3: %edx
	4: %esi
	5: %edi
   We put the arguments into registers from the stack,
   and save the registers, by using the 386 `xchg' instruction
   to swap the values in both directions.  */

#define	XCHG_0	/* No arguments to frob.  */
#define	XCHG_1	xchg 8(%esp), %ebx; XCHG_0
#define	XCHG_2	xchg 12(%esp), %ecx; XCHG_1
#define	XCHG_3	xchg 16(%esp), %edx; XCHG_2
#define	XCHG_4	xchg 20(%esp), %esi; XCHG_3
#define	XCHG_5	xchg 24(%esp), %edi; XCHG_3

#define	r0		%eax	/* Normal return-value register.  */
#define	r1		%edx	/* Secondary return-value register.  */
#define scratch 	%ecx	/* Call-clobbered register for random use.  */
#define MOVE(x,y)	movl x, y
