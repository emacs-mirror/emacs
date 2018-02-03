/* Header for CCL (Code Conversion Language) interpreter.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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


#ifndef EMACS_CCL_H
#define EMACS_CCL_H

/* Macros for exit status of CCL program.  */
#define CCL_STAT_SUCCESS	0 /* Terminated successfully.  */
#define CCL_STAT_SUSPEND_BY_SRC	1 /* Terminated by empty input.  */
#define CCL_STAT_SUSPEND_BY_DST	2 /* Terminated by output buffer full.  */
#define CCL_STAT_INVALID_CMD	3 /* Terminated because of invalid
				     command.  */
#define CCL_STAT_QUIT		4 /* Terminated because of quit.  */

/* Structure to hold information about running CCL code.  Read
   comments in the file ccl.c for the detail of each field.  */
struct ccl_program {
  ptrdiff_t idx;		/* Index number of the CCL program.
				   -1 means that the program was given
				   by a vector, not by a program
				   name.  */
  int size;			/* Size of the compiled code.  */
  Lisp_Object *prog;		/* Pointer into the compiled code.  */
  int ic;			/* Instruction Counter (index for PROG).  */
  int eof_ic;			/* Instruction Counter for end-of-file
				   processing code.  */
  int reg[8];			/* CCL registers, reg[7] is used for
				   condition flag of relational
				   operations.  */
  int status;			/* Exit status of the CCL program.  */
  int buf_magnification;	/* Output buffer magnification.  How
				   many times bigger the output buffer
				   should be than the input buffer.  */
  int stack_idx;		/* How deep the call of CCL_Call is nested.  */
  int consumed;
  int produced;
  bool_bf last_block : 1;	/* Set to true while processing the last
				   block. */
  bool_bf quit_silently : 1;	/* If true, don't append "CCL:
				   Quitted" to the generated text when
				   CCL program is quitted. */
};

/* This data type is used for the spec field of the structure
   coding_system.  */

struct ccl_spec {
  struct ccl_program ccl;
};

#define CODING_SPEC_CCL_PROGRAM(coding) ((coding)->spec.ccl.ccl)

/* Setup fields of the structure pointed by CCL appropriately for the
   execution of ccl program CCL_PROG (symbol or vector).  */
extern bool setup_ccl_program (struct ccl_program *, Lisp_Object);

extern void ccl_driver (struct ccl_program *, int *, int *, int, int,
                        Lisp_Object);

#define CHECK_CCL_PROGRAM(x)			\
  do {						\
    if (NILP (Fccl_program_p (x)))		\
      wrong_type_argument (Qcclp, (x));	\
  } while (false);

#endif /* EMACS_CCL_H */
