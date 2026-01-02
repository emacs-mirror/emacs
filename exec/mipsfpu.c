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

#include <config.h>
#include <errno.h>

#include "mipsfpu.h"



/* OABI MIPS systems support several different modes of execution.
   Each mode differs in the size and utilization of the hardware
   floating-point registers.

   Linux normally sets the floating point mode to one appropriate for
   execution, taking into account the floating point modes of the
   interpreter and executable binaries.  However, this logic is
   forsaken when the `execve' system call is overwritten.

   Thus, the correct floating point mode must be determined and set
   within the loader binary.  */



/* Various constants used throughout this code.  */

#define MIPS_ABI_FP_ANY		0	/* FP ABI doesn't matter */
#define MIPS_ABI_FP_DOUBLE	1	/* -mdouble-float */
#define MIPS_ABI_FP_SINGLE	2	/* -msingle-float */
#define MIPS_ABI_FP_SOFT	3	/* -msoft-float */
#define MIPS_ABI_FP_OLD_64	4	/* -mips32r2 -mfp64 */
#define MIPS_ABI_FP_XX		5	/* -mfpxx */
#define MIPS_ABI_FP_64		6	/* -mips32r2 -mfp64 */
#define MIPS_ABI_FP_64A		7	/* -mips32r2 -mfp64 -mno-odd-spreg */

#define EF_MIPS_NOREORDER	1     /* A .noreorder directive was used.  */
#define EF_MIPS_PIC		2     /* Contains PIC code.  */
#define EF_MIPS_CPIC		4     /* Uses PIC calling sequence.  */
#define EF_MIPS_XGOT		8
#define EF_MIPS_64BIT_WHIRL	16
#define EF_MIPS_ABI2		32
#define EF_MIPS_ABI_ON32	64
#define EF_MIPS_FP64		512  /* Uses FP64 (12 callee-saved).  */
#define EF_MIPS_NAN2008		1024  /* Uses IEEE 754-2008 NaN encoding.  */
#define EF_MIPS_ARCH		0xf0000000 /* MIPS architecture level.  */



/* Structure describing the requirements of a single floating-point
   ABI.  */

struct mode_description
{
  /* Whether or not the ABI only executes single precision
     instructions, and can operate in both 32-bit or 64-bit floating
     point mode.  */
  bool single;

  /* Whether or not the ABI performs floating point operations in
     software, using integer registers.  */
  bool soft;

  /* Whether or not the ABI requires the use of 64-bit floating point
     registers.  */
  bool fr1;

  /* Whether or not the ABI requires the use of 64-bit floating point
     registers on NABI systems, and 32-bit ones on OABI systems.  */
  bool frdefault;

  /* Whether or not this ABI requires single precision floating point
     emulation.  */
  bool fre;
};

static struct mode_description fpu_reqs[] =
  {
    [MIPS_ABI_FP_ANY]    = { true,  true,  true,  true,  true,  },
    [MIPS_ABI_FP_DOUBLE] = { false, false, false, true,  true,  },
    [MIPS_ABI_FP_SINGLE] = { true,  false, false, false, false, },
    [MIPS_ABI_FP_SOFT]   = { false, true,  false, false, false, },
    [MIPS_ABI_FP_OLD_64] = { false, false, false, false, false, },
    [MIPS_ABI_FP_XX]     = { false, false, true,  true,  true,  },
    [MIPS_ABI_FP_64]     = { false, false, true,  false, false, },
    [MIPS_ABI_FP_64A]    = { false, false, true,  false, true,  },
  };



/* Return whether or not the given floating-point ABI is valid.  */

static bool
valid_abi_p (int abi)
{
  switch (abi)
    {
    case MIPS_ABI_FP_ANY:
    case MIPS_ABI_FP_DOUBLE:
    case MIPS_ABI_FP_SINGLE:
    case MIPS_ABI_FP_SOFT:
    case MIPS_ABI_FP_OLD_64:
    case MIPS_ABI_FP_XX:
    case MIPS_ABI_FP_64:
    case MIPS_ABI_FP_64A:
      return true;

    default:
      return false;
    }
}

/* Return the floating point mode appropriate for the specified
   floating point ABI.  */

static int
fp_mode_for_abi (int abi)
{
  struct mode_description *desc;

  desc = &fpu_reqs[abi];

  if (desc->fre)
    return FP_FRE;
  else if (desc->fr1)
    return FP_FR1;

  return FP_FR0;
}

/* Determine whether or not the CPU is capable of operating in FR0
   floating point mode.  */

bool
cpu_supports_fr0_p (void)
{
#if defined __mips_isa_rev && __mips_isa_rev >= 6
  return true;
#else /* !defined __mips_isa_rev | mips_isa_rev < 6 */
  return false;
#endif /* defined __mips_isa_rev && mips_isa_rev >= 6 */
}

/* Determine the FPU mode for the executable whose ELF header is
   HEADER.  If INTERPRETER is non-NULL, also take an interpreter whose
   header is INTERPRETER into account.

   ABIFLAGS should be HEADER's corresponding PT_MIPS_ABIFLAGS program
   header, and ABIFLAGS1 should be that of INTERPRETER, if set.  Both
   fields may be NULL if no PT_MIPS_ABIFLAGS header is present; in
   that case, use HEADER->e_flags to determine the ABI instead.

   Return the FPU mode in *MODE.  Value is 0 upon success, 1
   otherwise, with errno set.  */

int
determine_fpu_mode (elf_header *header, elf_header *interpreter,
		    int *mode, struct mips_elf_abi_flags *abiflags,
		    struct mips_elf_abi_flags *abiflags1)
{
  int exec_abi, interpreter_abi;
  struct mode_description *exec_desc, *interpreter_desc, common;

  /* Figure out the executable's floating point ABI.  First, consult
     header->e_flags, and use the old 64-bit floating point ABI if it
     is specified.  */

  exec_abi = MIPS_ABI_FP_ANY;

  /* First, check HEADER->e_flags.  */

  if (header->e_flags & EF_MIPS_FP64)
    exec_abi = MIPS_ABI_FP_OLD_64;

  /* Next, use ABIFLAGS if it exists.  */

  if (abiflags && valid_abi_p (abiflags->fp_abi))
    exec_abi = abiflags->fp_abi;
  else if (abiflags)
    {
      errno = ENOEXEC;
      return 1;
    }

  /* Now determine that of the interpreter.  */

  interpreter_abi = MIPS_ABI_FP_ANY;

  if (interpreter)
    {
      if (interpreter->e_flags & EF_MIPS_FP64)
	interpreter_abi = MIPS_ABI_FP_OLD_64;

      if (abiflags1 && valid_abi_p (abiflags->fp_abi))
	interpreter_abi = abiflags->fp_abi;
      else if (abiflags1)
	{
	  errno = ELIBBAD;
	  return 1;
	}
    }

  /* If no interpreter flag is set, just return that of the
     executable.  */

  if (!interpreter)
    {
      *mode = fp_mode_for_abi (exec_abi);
      return 0;
    }

  /* Otherwise, compare both ABIs and try to find one which will run
     both kinds of code.

     First, see if there's an easy way out: both ABIs are identical,
     or one ABI is MIPS_ABI_FP_ANY.  */

  if (exec_abi == interpreter_abi)
    {
      *mode = fp_mode_for_abi (exec_abi);
      return 0;
    }
  else if (exec_abi == MIPS_ABI_FP_ANY)
    {
      *mode = fp_mode_for_abi (interpreter_abi);
      return 0;
    }
  else if (interpreter_abi == MIPS_ABI_FP_ANY)
    {
      *mode = fp_mode_for_abi (exec_abi);
      return 0;
    }

  /* If that doesn't work, compare various characteristics of both
     ABIs and select an appropriate floating point mode.  */

  exec_desc = &fpu_reqs[exec_abi];
  interpreter_desc = &fpu_reqs[interpreter_abi];

  /* Merge both sets of requirements.  */
  common.single = exec_desc->single && interpreter_desc->single;
  common.soft = exec_desc->soft && interpreter_desc->soft;
  common.fr1 = exec_desc->fr1 && interpreter_desc->fr1;
  common.frdefault = exec_desc->frdefault && interpreter_desc->frdefault;
  common.fre = exec_desc->fre && interpreter_desc->fre;

  /* Default to a mode capable of running code expecting 32-bit
     registers.  */

  if (!(header->e_flags & EF_MIPS_ABI2))
    *mode = FP_FR0;
  else
    /* But in this case, use FR1.  */
    *mode = FP_FR1;

  if (common.fre && !common.frdefault && !common.fr1)
    /* Floating point emulation mode is required.  */
    *mode = FP_FRE;
  else if ((common.fr1 && common.frdefault)
	   || (common.single && !common.frdefault)
	   || common.fr1)
    /* 64-bit mode is required.  */
    *mode = FP_FR1;
  else if (!common.fre && !common.frdefault
	   && !common.fr1 && !common.single
	   && !common.soft)
    {
      /* The floating point modes specified are incompatible.  */
      errno = ELIBBAD;
      return -1;
    }

  return 0;
}
