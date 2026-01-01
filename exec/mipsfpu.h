/* Program execution for Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */



#ifndef _MIPSFPU_H_
#define _MIPSFPU_H_

#include "exec.h"

struct mips_elf_abi_flags
{
  /* Version of flags structure.  */
  uint16_t version;

  /* The level of the ISA: 1-5, 32, 64.  */
  uint8_t isa_level;

  /* The revision of ISA: 0 for MIPS V and below, 1-n otherwise.  */
  uint8_t isa_rev;

  /* The size of general purpose registers.  */
  uint8_t gpr_size;

  /* The size of co-processor 1 registers.  */
  uint8_t cpr1_size;

  /* The size of co-processor 2 registers.  */
  uint8_t cpr2_size;

  /* The floating-point ABI.  */
  uint8_t fp_abi;

  /* Mask of processor-specific extensions.  */
  uint32_t isa_ext;

  /* Mask of ASEs used.  */
  uint32_t ases;

  /* Mask of general flags.  */
  uint32_t flags1;

  /* Mask of general flags.  */
  uint32_t flags2;
};



/* Floating point modes.  */

#define FP_FR0		0
#define FP_FR1		1
#define FP_FRE		3



/* Defined in mipsfpu.c.  */

extern bool cpu_supports_fr0_p (void);
extern int determine_fpu_mode (elf_header *, elf_header *,
			       int *, struct mips_elf_abi_flags *,
			       struct mips_elf_abi_flags *);



#endif /* _MIPSFPU_H_ */
