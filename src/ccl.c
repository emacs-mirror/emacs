/* CCL (Code Conversion Language) interpreter.
   Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <stdio.h>

#ifdef emacs

#include <config.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "lisp.h"
#include "charset.h"
#include "ccl.h"
#include "coding.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* not emacs */

/* Where is stored translation tables for CCL program.  */
Lisp_Object Vccl_translation_table_vector;

/* Alist of fontname patterns vs corresponding CCL program.  */
Lisp_Object Vfont_ccl_encoder_alist;

/* This symbol is property which assocate with ccl program vector. e.g.
   (get 'ccl-big5-encoder 'ccl-program) returns ccl program vector */
Lisp_Object Qccl_program;

/* These symbol is properties whish associate with ccl translation table and its id
   respectively.  */
Lisp_Object Qccl_translation_table;
Lisp_Object Qccl_translation_table_id;

/* Vector of CCL program names vs corresponding program data.  */
Lisp_Object Vccl_program_table;

/* CCL (Code Conversion Language) is a simple language which has
   operations on one input buffer, one output buffer, and 7 registers.
   The syntax of CCL is described in `ccl.el'.  Emacs Lisp function
   `ccl-compile' compiles a CCL program and produces a CCL code which
   is a vector of integers.  The structure of this vector is as
   follows: The 1st element: buffer-magnification, a factor for the
   size of output buffer compared with the size of input buffer.  The
   2nd element: address of CCL code to be executed when encountered
   with end of input stream.  The 3rd and the remaining elements: CCL
   codes.  */

/* Header of CCL compiled code */
#define CCL_HEADER_BUF_MAG	0
#define CCL_HEADER_EOF		1
#define CCL_HEADER_MAIN		2

/* CCL code is a sequence of 28-bit non-negative integers (i.e. the
   MSB is always 0), each contains CCL command and/or arguments in the
   following format:

	|----------------- integer (28-bit) ------------------|
	|------- 17-bit ------|- 3-bit --|- 3-bit --|- 5-bit -|
	|--constant argument--|-register-|-register-|-command-|
	   ccccccccccccccccc      RRR        rrr       XXXXX
  or
	|------- relative address -------|-register-|-command-|
	       cccccccccccccccccccc          rrr       XXXXX
  or
	|------------- constant or other args ----------------|
                     cccccccccccccccccccccccccccc

   where, `cc...c' is a non-negative integer indicating constant value
   (the left most `c' is always 0) or an absolute jump address, `RRR'
   and `rrr' are CCL register number, `XXXXX' is one of the following
   CCL commands.  */

/* CCL commands

   Each comment fields shows one or more lines for command syntax and
   the following lines for semantics of the command.  In semantics, IC
   stands for Instruction Counter.  */

#define CCL_SetRegister		0x00 /* Set register a register value:
					1:00000000000000000RRRrrrXXXXX
					------------------------------
					reg[rrr] = reg[RRR];
					*/

#define CCL_SetShortConst	0x01 /* Set register a short constant value:
					1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					------------------------------
					reg[rrr] = CCCCCCCCCCCCCCCCCCC;
					*/

#define CCL_SetConst		0x02 /* Set register a constant value:
					1:00000000000000000000rrrXXXXX
					2:CONSTANT
					------------------------------
					reg[rrr] = CONSTANT;
					IC++;
					*/

#define CCL_SetArray		0x03 /* Set register an element of array:
					1:CCCCCCCCCCCCCCCCCRRRrrrXXXXX
					2:ELEMENT[0]
					3:ELEMENT[1]
					...
					------------------------------
					if (0 <= reg[RRR] < CC..C)
					  reg[rrr] = ELEMENT[reg[RRR]];
					IC += CC..C;
					*/

#define CCL_Jump		0x04 /* Jump:
					1:A--D--D--R--E--S--S-000XXXXX
					------------------------------
					IC += ADDRESS;
					*/

/* Note: If CC..C is greater than 0, the second code is omitted.  */

#define CCL_JumpCond		0x05 /* Jump conditional:
					1:A--D--D--R--E--S--S-rrrXXXXX
					------------------------------
					if (!reg[rrr])
					  IC += ADDRESS;
					*/


#define CCL_WriteRegisterJump	0x06 /* Write register and jump:
					1:A--D--D--R--E--S--S-rrrXXXXX
					------------------------------
					write (reg[rrr]);
					IC += ADDRESS;
					*/

#define CCL_WriteRegisterReadJump 0x07 /* Write register, read, and jump:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:A--D--D--R--E--S--S-rrrYYYYY
					-----------------------------
					write (reg[rrr]);
					IC++;
					read (reg[rrr]);
					IC += ADDRESS;
					*/
/* Note: If read is suspended, the resumed execution starts from the
   second code (YYYYY == CCL_ReadJump).  */

#define CCL_WriteConstJump	0x08 /* Write constant and jump:
					1:A--D--D--R--E--S--S-000XXXXX
					2:CONST
					------------------------------
					write (CONST);
					IC += ADDRESS;
					*/

#define CCL_WriteConstReadJump	0x09 /* Write constant, read, and jump:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:CONST
					3:A--D--D--R--E--S--S-rrrYYYYY
					-----------------------------
					write (CONST);
					IC += 2;
					read (reg[rrr]);
					IC += ADDRESS;
					*/
/* Note: If read is suspended, the resumed execution starts from the
   second code (YYYYY == CCL_ReadJump).  */

#define CCL_WriteStringJump	0x0A /* Write string and jump:
					1:A--D--D--R--E--S--S-000XXXXX
					2:LENGTH
					3:0000STRIN[0]STRIN[1]STRIN[2]
					...
					------------------------------
					write_string (STRING, LENGTH);
					IC += ADDRESS;
					*/

#define CCL_WriteArrayReadJump	0x0B /* Write an array element, read, and jump:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:LENGTH
					3:ELEMENET[0]
					4:ELEMENET[1]
					...
					N:A--D--D--R--E--S--S-rrrYYYYY
					------------------------------
					if (0 <= reg[rrr] < LENGTH)
					  write (ELEMENT[reg[rrr]]);
					IC += LENGTH + 2; (... pointing at N+1)
					read (reg[rrr]);
					IC += ADDRESS;
					*/
/* Note: If read is suspended, the resumed execution starts from the
   Nth code (YYYYY == CCL_ReadJump).  */

#define CCL_ReadJump		0x0C /* Read and jump:
					1:A--D--D--R--E--S--S-rrrYYYYY
					-----------------------------
					read (reg[rrr]);
					IC += ADDRESS;
					*/

#define CCL_Branch		0x0D /* Jump by branch table:
					1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					2:A--D--D--R--E-S-S[0]000XXXXX
					3:A--D--D--R--E-S-S[1]000XXXXX
					...
					------------------------------
					if (0 <= reg[rrr] < CC..C)
					  IC += ADDRESS[reg[rrr]];
					else
					  IC += ADDRESS[CC..C];
					*/

#define CCL_ReadRegister	0x0E /* Read bytes into registers:
					1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					2:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					...
					------------------------------
					while (CCC--)
					  read (reg[rrr]);
					*/

#define CCL_WriteExprConst	0x0F  /* write result of expression:
					1:00000OPERATION000RRR000XXXXX
					2:CONSTANT
					------------------------------
					write (reg[RRR] OPERATION CONSTANT);
					IC++;
					*/

/* Note: If the Nth read is suspended, the resumed execution starts
   from the Nth code.  */

#define CCL_ReadBranch		0x10 /* Read one byte into a register,
					and jump by branch table:
					1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					2:A--D--D--R--E-S-S[0]000XXXXX
					3:A--D--D--R--E-S-S[1]000XXXXX
					...
					------------------------------
					read (read[rrr]);
					if (0 <= reg[rrr] < CC..C)
					  IC += ADDRESS[reg[rrr]];
					else
					  IC += ADDRESS[CC..C];
					*/

#define CCL_WriteRegister	0x11 /* Write registers:
					1:CCCCCCCCCCCCCCCCCCCrrrXXXXX
					2:CCCCCCCCCCCCCCCCCCCrrrXXXXX
					...
					------------------------------
					while (CCC--)
					  write (reg[rrr]);
					...
					*/

/* Note: If the Nth write is suspended, the resumed execution
   starts from the Nth code.  */

#define CCL_WriteExprRegister	0x12 /* Write result of expression
					1:00000OPERATIONRrrRRR000XXXXX
					------------------------------
					write (reg[RRR] OPERATION reg[Rrr]);
					*/

#define CCL_Call		0x13 /* Call the CCL program whose ID is
					(CC..C).
					1:CCCCCCCCCCCCCCCCCCCC000XXXXX
					------------------------------
					call (CC..C)
					*/

#define CCL_WriteConstString	0x14 /* Write a constant or a string:
					1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					[2:0000STRIN[0]STRIN[1]STRIN[2]]
					[...]
					-----------------------------
					if (!rrr)
					  write (CC..C)
					else
					  write_string (STRING, CC..C);
					  IC += (CC..C + 2) / 3;
					*/

#define CCL_WriteArray		0x15 /* Write an element of array:
					1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					2:ELEMENT[0]
					3:ELEMENT[1]
					...
					------------------------------
					if (0 <= reg[rrr] < CC..C)
					  write (ELEMENT[reg[rrr]]);
					IC += CC..C;
					*/

#define CCL_End			0x16 /* Terminate:
					1:00000000000000000000000XXXXX
					------------------------------
					terminate ();
					*/

/* The following two codes execute an assignment arithmetic/logical
   operation.  The form of the operation is like REG OP= OPERAND.  */

#define CCL_ExprSelfConst	0x17 /* REG OP= constant:
					1:00000OPERATION000000rrrXXXXX
					2:CONSTANT
					------------------------------
					reg[rrr] OPERATION= CONSTANT;
					*/

#define CCL_ExprSelfReg		0x18 /* REG1 OP= REG2:
					1:00000OPERATION000RRRrrrXXXXX
					------------------------------
					reg[rrr] OPERATION= reg[RRR];
					*/

/* The following codes execute an arithmetic/logical operation.  The
   form of the operation is like REG_X = REG_Y OP OPERAND2.  */

#define CCL_SetExprConst	0x19 /* REG_X = REG_Y OP constant:
					1:00000OPERATION000RRRrrrXXXXX
					2:CONSTANT
					------------------------------
					reg[rrr] = reg[RRR] OPERATION CONSTANT;
					IC++;
					*/

#define CCL_SetExprReg		0x1A /* REG1 = REG2 OP REG3:
					1:00000OPERATIONRrrRRRrrrXXXXX
					------------------------------
					reg[rrr] = reg[RRR] OPERATION reg[Rrr];
					*/

#define CCL_JumpCondExprConst	0x1B /* Jump conditional according to
					an operation on constant:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:OPERATION
					3:CONSTANT
					-----------------------------
					reg[7] = reg[rrr] OPERATION CONSTANT;
					if (!(reg[7]))
					  IC += ADDRESS;
					else
					  IC += 2
					*/

#define CCL_JumpCondExprReg	0x1C /* Jump conditional according to
					an operation on register:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:OPERATION
					3:RRR
					-----------------------------
					reg[7] = reg[rrr] OPERATION reg[RRR];
					if (!reg[7])
					  IC += ADDRESS;
					else
					  IC += 2;
					*/

#define CCL_ReadJumpCondExprConst 0x1D /* Read and jump conditional according
					  to an operation on constant:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:OPERATION
					3:CONSTANT
					-----------------------------
					read (reg[rrr]);
					reg[7] = reg[rrr] OPERATION CONSTANT;
					if (!reg[7])
					  IC += ADDRESS;
					else
					  IC += 2;
					*/

#define CCL_ReadJumpCondExprReg	0x1E /* Read and jump conditional according
					to an operation on register:
					1:A--D--D--R--E--S--S-rrrXXXXX
					2:OPERATION
					3:RRR
					-----------------------------
					read (reg[rrr]);
					reg[7] = reg[rrr] OPERATION reg[RRR];
					if (!reg[7])
					  IC += ADDRESS;
					else
					  IC += 2;
					*/

#define CCL_Extention		0x1F /* Extended CCL code
					1:ExtendedCOMMNDRrrRRRrrrXXXXX
					2:ARGUEMENT
					3:...
					------------------------------
					extended_command (rrr,RRR,Rrr,ARGS)
				      */

/* 
   From here, Extended CCL Instruction.
   Bit length of extended command is 14.
   Therefore the instruction code begins from 0 to 16384(0x3fff).
 */

#define CCL_ReadMultibyteCharacter  0x00 /* Read Multibyte Character
					    1:ExtendedCOMMNDRrrRRRrrrXXXXX

					    Read a multibyte characeter.
					    A code point is stored
					    into rrr register. 
					    A charset ID is stored
					    into RRR register.
					 */
#define CCL_WriteMultibyteCharacter 0x01 /* Write Multibyte Character
					    1:ExtendedCOMMNDRrrRRRrrrXXXXX

					    Write a multibyte character.
					    Write a character whose code point
					    is in rrr register, and its charset ID
					    is in RRR charset.
					 */
#define CCL_UnifyCharacter          0x02 /* Unify Multibyte Character
					    1:ExtendedCOMMNDRrrRRRrrrXXXXX

					    Unify a character where its code point
					    is in rrr register, and its charset ID
					    is in RRR register with the table of
					    the unification table ID
					    in Rrr register.

					    Return a unified character where its
					    code point is in rrr register, and its
					    charset ID is in RRR register.
					 */
#define CCL_UnifyCharacterConstTbl  0x03 /* Unify Multibyte Character
					    1:ExtendedCOMMNDRrrRRRrrrXXXXX
					    2:ARGUMENT(Unification Table ID)

					    Unify a character where its code point
					    is in rrr register, and its charset ID
					    is in RRR register with the table of
					    the unification table ID
					    in 2nd argument.

					    Return a unified character where its
					    code point is in rrr register, and its
					    charset ID is in RRR register.
					 */
#define CCL_IterateMultipleMap      0x10 /* Iterate Multiple Map
					    1:ExtendedCOMMNDXXXRRRrrrXXXXX
					    2:NUMBER of TABLES
					    3:TABLE-ID1
					    4:TABLE-ID2
					    ...
					    
					    iterate to lookup tables from a number
					    until finding a value.

					    Each table consists of a vector
					    whose element is number or
					    nil or t or lambda.
					    If the element is nil,
					    its table is neglected.
					    In the case of t or lambda,
					    return the original value.
					    
					  */
#define CCL_TranslateMultipleMap    0x11 /* Translate Multiple Map
					    1:ExtendedCOMMNDXXXRRRrrrXXXXX
					    2:NUMBER of TABLE-IDs and SEPARATERs
					    (i.e. m1+m2+m3+...mk+k-1)
					    3:TABLE-ID 1,1
					    4:TABLE-ID 1,2
					    ...
					    m1+2:TABLE-ID 1,m1
					    m1+3: -1     (SEPARATOR)
					    m1+4:TABLE-ID 2,1
					    ...
					    m1+m2+4:TABLE-ID 2,m2
					    m1+m2+5: -1
					    ...
					    m1+m2+...+mk+k+1:TABLE-ID k,mk
					    
					    Translate the code point in
					    rrr register by tables.
					    Translation starts from the table
					    where RRR register points out.

					    We translate the given value
					    from the tables which are separated
					    by -1.
					    When each translation is failed to find
					    any values, we regard the traslation
					    as identity.

					    We iterate to traslate by using each
					    table set(tables separated by -1)
					    until lookup the last table except
					    lookup lambda.

					    Each table consists of a vector
					    whose element is number
					    or nil or t or lambda.
					    If the element is nil,
					    it is neglected and use the next table.
					    In the case of t,
					    it is translated to the original value.
					    In the case of lambda,
					    it cease the translation and return the
					    current value.

					  */
#define CCL_TranslateSingleMap       0x12 /* Translate Single Map
					    1:ExtendedCOMMNDXXXRRRrrrXXXXX
					    2:TABLE-ID
					    
					    Translate a number in rrr register.
					    If it is not found any translation,
					    set RRR register -1 but rrr register
					    is not changed.
					  */

/* CCL arithmetic/logical operators. */
#define CCL_PLUS	0x00	/* X = Y + Z */
#define CCL_MINUS	0x01	/* X = Y - Z */
#define CCL_MUL		0x02	/* X = Y * Z */
#define CCL_DIV		0x03	/* X = Y / Z */
#define CCL_MOD		0x04	/* X = Y % Z */
#define CCL_AND		0x05	/* X = Y & Z */
#define CCL_OR		0x06	/* X = Y | Z */
#define CCL_XOR		0x07	/* X = Y ^ Z */
#define CCL_LSH		0x08	/* X = Y << Z */
#define CCL_RSH		0x09	/* X = Y >> Z */
#define CCL_LSH8	0x0A	/* X = (Y << 8) | Z */
#define CCL_RSH8	0x0B	/* X = Y >> 8, r[7] = Y & 0xFF  */
#define CCL_DIVMOD	0x0C	/* X = Y / Z, r[7] = Y % Z */
#define CCL_LS		0x10	/* X = (X < Y) */
#define CCL_GT		0x11	/* X = (X > Y) */
#define CCL_EQ		0x12	/* X = (X == Y) */
#define CCL_LE		0x13	/* X = (X <= Y) */
#define CCL_GE		0x14	/* X = (X >= Y) */
#define CCL_NE		0x15	/* X = (X != Y) */

#define CCL_ENCODE_SJIS 0x16	/* X = HIGHER_BYTE (SJIS (Y, Z))
				   r[7] = LOWER_BYTE (SJIS (Y, Z) */
#define CCL_DECODE_SJIS 0x17	/* X = HIGHER_BYTE (DE-SJIS (Y, Z))
				   r[7] = LOWER_BYTE (DE-SJIS (Y, Z)) */

/* Terminate CCL program successfully.  */
#define CCL_SUCCESS		   	\
  do {				   	\
    ccl->status = CCL_STAT_SUCCESS;	\
    ccl->ic = CCL_HEADER_MAIN;		\
    goto ccl_finish;		   	\
  } while (0)

/* Suspend CCL program because of reading from empty input buffer or
   writing to full output buffer.  When this program is resumed, the
   same I/O command is executed.  */
#define CCL_SUSPEND(stat)	\
  do {				\
    ic--;			\
    ccl->status = stat;		\
    goto ccl_finish;		\
  } while (0)

/* Terminate CCL program because of invalid command.  Should not occur
   in the normal case.  */
#define CCL_INVALID_CMD		     	\
  do {				     	\
    ccl->status = CCL_STAT_INVALID_CMD;	\
    goto ccl_error_handler;	     	\
  } while (0)

/* Encode one character CH to multibyte form and write to the current
   output buffer.  If CH is less than 256, CH is written as is.  */
#define CCL_WRITE_CHAR(ch)				\
  do {							\
    if (!dst)						\
      CCL_INVALID_CMD;					\
    else						\
      {							\
	unsigned char work[4], *str;			\
	int len = CHAR_STRING (ch, work, str);		\
	if (dst + len <= (dst_bytes ? dst_end : src))	\
	  {						\
	    bcopy (str, dst, len);			\
	    dst += len;					\
	  }						\
	else						\
	  CCL_SUSPEND (CCL_STAT_SUSPEND_BY_DST);	\
      }							\
  } while (0)

/* Write a string at ccl_prog[IC] of length LEN to the current output
   buffer.  */
#define CCL_WRITE_STRING(len)				\
  do {							\
    if (!dst)						\
      CCL_INVALID_CMD;					\
    else if (dst + len <= (dst_bytes ? dst_end : src))	\
      for (i = 0; i < len; i++)				\
	*dst++ = ((XFASTINT (ccl_prog[ic + (i / 3)]))	\
		  >> ((2 - (i % 3)) * 8)) & 0xFF;	\
    else						\
      CCL_SUSPEND (CCL_STAT_SUSPEND_BY_DST);		\
  } while (0)

/* Read one byte from the current input buffer into Rth register.  */
#define CCL_READ_CHAR(r)			\
  do {						\
    if (!src)					\
      CCL_INVALID_CMD;				\
    else if (src < src_end)			\
      r = *src++;				\
    else if (ccl->last_block)			\
      {						\
        ic = ccl->eof_ic;			\
        goto ccl_finish;			\
      }						\
    else					\
      CCL_SUSPEND (CCL_STAT_SUSPEND_BY_SRC);	\
  } while (0)


/* Execute CCL code on SRC_BYTES length text at SOURCE.  The resulting
   text goes to a place pointed by DESTINATION, the length of which
   should not exceed DST_BYTES.  The bytes actually processed is
   returned as *CONSUMED.  The return value is the length of the
   resulting text.  As a side effect, the contents of CCL registers
   are updated.  If SOURCE or DESTINATION is NULL, only operations on
   registers are permitted.  */

#ifdef CCL_DEBUG
#define CCL_DEBUG_BACKTRACE_LEN 256
int ccl_backtrace_table[CCL_BACKTRACE_TABLE];
int ccl_backtrace_idx;
#endif

struct ccl_prog_stack
  {
    Lisp_Object *ccl_prog;	/* Pointer to an array of CCL code.  */
    int ic;			/* Instruction Counter.  */
  };

int
ccl_driver (ccl, source, destination, src_bytes, dst_bytes, consumed)
     struct ccl_program *ccl;
     unsigned char *source, *destination;
     int src_bytes, dst_bytes;
     int *consumed;
{
  register int *reg = ccl->reg;
  register int ic = ccl->ic;
  register int code, field1, field2;
  register Lisp_Object *ccl_prog = ccl->prog;
  unsigned char *src = source, *src_end = src + src_bytes;
  unsigned char *dst = destination, *dst_end = dst + dst_bytes;
  int jump_address;
  int i, j, op;
  int stack_idx = 0;
  /* For the moment, we only support depth 256 of stack.  */ 
  struct ccl_prog_stack ccl_prog_stack_struct[256];

  if (ic >= ccl->eof_ic)
    ic = CCL_HEADER_MAIN;

#ifdef CCL_DEBUG
  ccl_backtrace_idx = 0;
#endif

  for (;;)
    {
#ifdef CCL_DEBUG
      ccl_backtrace_table[ccl_backtrace_idx++] = ic;
      if (ccl_backtrace_idx >= CCL_DEBUG_BACKTRACE_LEN)
	ccl_backtrace_idx = 0;
      ccl_backtrace_table[ccl_backtrace_idx] = 0;
#endif

      if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))
	{
	  /* We can't just signal Qquit, instead break the loop as if
             the whole data is processed.  Don't reset Vquit_flag, it
             must be handled later at a safer place.  */
	  if (consumed)
	    src = source + src_bytes;
	  ccl->status = CCL_STAT_QUIT;
	  break;
	}

      code = XINT (ccl_prog[ic]); ic++;
      field1 = code >> 8;
      field2 = (code & 0xFF) >> 5;

#define rrr field2
#define RRR (field1 & 7)
#define Rrr ((field1 >> 3) & 7)
#define ADDR field1
#define EXCMD (field1 >> 6)

      switch (code & 0x1F)
	{
	case CCL_SetRegister:	/* 00000000000000000RRRrrrXXXXX */
	  reg[rrr] = reg[RRR];
	  break;

	case CCL_SetShortConst:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
	  reg[rrr] = field1;
	  break;

	case CCL_SetConst:	/* 00000000000000000000rrrXXXXX */
	  reg[rrr] = XINT (ccl_prog[ic]);
	  ic++;
	  break;

	case CCL_SetArray:	/* CCCCCCCCCCCCCCCCCCCCRRRrrrXXXXX */
	  i = reg[RRR];
	  j = field1 >> 3;
	  if ((unsigned int) i < j)
	    reg[rrr] = XINT (ccl_prog[ic + i]);
	  ic += j;
	  break;

	case CCL_Jump:		/* A--D--D--R--E--S--S-000XXXXX */
	  ic += ADDR;
	  break;

	case CCL_JumpCond:	/* A--D--D--R--E--S--S-rrrXXXXX */
	  if (!reg[rrr])
	    ic += ADDR;
	  break;

	case CCL_WriteRegisterJump: /* A--D--D--R--E--S--S-rrrXXXXX */
	  i = reg[rrr];
	  CCL_WRITE_CHAR (i);
	  ic += ADDR;
	  break;

	case CCL_WriteRegisterReadJump: /* A--D--D--R--E--S--S-rrrXXXXX */
	  i = reg[rrr];
	  CCL_WRITE_CHAR (i);
	  ic++;
	  CCL_READ_CHAR (reg[rrr]);
	  ic += ADDR - 1;
	  break;

	case CCL_WriteConstJump: /* A--D--D--R--E--S--S-000XXXXX */
	  i = XINT (ccl_prog[ic]);
	  CCL_WRITE_CHAR (i);
	  ic += ADDR;
	  break;

	case CCL_WriteConstReadJump: /* A--D--D--R--E--S--S-rrrXXXXX */
	  i = XINT (ccl_prog[ic]);
	  CCL_WRITE_CHAR (i);
	  ic++;
	  CCL_READ_CHAR (reg[rrr]);
	  ic += ADDR - 1;
	  break;

	case CCL_WriteStringJump: /* A--D--D--R--E--S--S-000XXXXX */
	  j = XINT (ccl_prog[ic]);
	  ic++;
	  CCL_WRITE_STRING (j);
	  ic += ADDR - 1;
	  break;

	case CCL_WriteArrayReadJump: /* A--D--D--R--E--S--S-rrrXXXXX */
	  i = reg[rrr];
	  j = XINT (ccl_prog[ic]);
	  if ((unsigned int) i < j)
	    {
	      i = XINT (ccl_prog[ic + 1 + i]);
	      CCL_WRITE_CHAR (i);
	    }
	  ic += j + 2;
	  CCL_READ_CHAR (reg[rrr]);
	  ic += ADDR - (j + 2);
	  break;

	case CCL_ReadJump:	/* A--D--D--R--E--S--S-rrrYYYYY */
	  CCL_READ_CHAR (reg[rrr]);
	  ic += ADDR;
	  break;

	case CCL_ReadBranch:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
	  CCL_READ_CHAR (reg[rrr]);
	  /* fall through ... */
	case CCL_Branch:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
	  if ((unsigned int) reg[rrr] < field1)
	    ic += XINT (ccl_prog[ic + reg[rrr]]);
	  else
	    ic += XINT (ccl_prog[ic + field1]);
	  break;

	case CCL_ReadRegister:	/* CCCCCCCCCCCCCCCCCCCCrrXXXXX */
	  while (1)
	    {
	      CCL_READ_CHAR (reg[rrr]);
	      if (!field1) break;
	      code = XINT (ccl_prog[ic]); ic++;
	      field1 = code >> 8;
	      field2 = (code & 0xFF) >> 5;
	    }
	  break;

	case CCL_WriteExprConst:  /* 1:00000OPERATION000RRR000XXXXX */
	  rrr = 7;
	  i = reg[RRR];
	  j = XINT (ccl_prog[ic]);
	  op = field1 >> 6;
	  ic++;
	  goto ccl_set_expr;

	case CCL_WriteRegister:	/* CCCCCCCCCCCCCCCCCCCrrrXXXXX */
	  while (1)
	    {
	      i = reg[rrr];
	      CCL_WRITE_CHAR (i);
	      if (!field1) break;
	      code = XINT (ccl_prog[ic]); ic++;
	      field1 = code >> 8;
	      field2 = (code & 0xFF) >> 5;
	    }
	  break;

	case CCL_WriteExprRegister: /* 1:00000OPERATIONRrrRRR000XXXXX */
	  rrr = 7;
	  i = reg[RRR];
	  j = reg[Rrr];
	  op = field1 >> 6;
	  goto ccl_set_expr;

	case CCL_Call:		/* CCCCCCCCCCCCCCCCCCCC000XXXXX */
	  {
	    Lisp_Object slot;

	    if (stack_idx >= 256
		|| field1 < 0
		|| field1 >= XVECTOR (Vccl_program_table)->size
		|| (slot = XVECTOR (Vccl_program_table)->contents[field1],
		    !CONSP (slot))
		|| !VECTORP (XCONS (slot)->cdr))
	      {
		if (stack_idx > 0)
		  {
		    ccl_prog = ccl_prog_stack_struct[0].ccl_prog;
		    ic = ccl_prog_stack_struct[0].ic;
		  }
		CCL_INVALID_CMD;
	      }
	    
	    ccl_prog_stack_struct[stack_idx].ccl_prog = ccl_prog;
	    ccl_prog_stack_struct[stack_idx].ic = ic;
	    stack_idx++;
	    ccl_prog = XVECTOR (XCONS (slot)->cdr)->contents;
	    ic = CCL_HEADER_MAIN;
	  }
	  break;

	case CCL_WriteConstString: /* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
	  if (!rrr)
	    CCL_WRITE_CHAR (field1);
	  else
	    {
	      CCL_WRITE_STRING (field1);
	      ic += (field1 + 2) / 3;
	    }
	  break;

	case CCL_WriteArray:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
	  i = reg[rrr];
	  if ((unsigned int) i < field1)
	    {
	      j = XINT (ccl_prog[ic + i]);
	      CCL_WRITE_CHAR (j);
	    }
	  ic += field1;
	  break;

	case CCL_End:		/* 0000000000000000000000XXXXX */
	  if (stack_idx-- > 0)
	    {
	      ccl_prog = ccl_prog_stack_struct[stack_idx].ccl_prog;
	      ic = ccl_prog_stack_struct[stack_idx].ic;
	      break;
	    }
	  CCL_SUCCESS;

	case CCL_ExprSelfConst: /* 00000OPERATION000000rrrXXXXX */
	  i = XINT (ccl_prog[ic]);
	  ic++;
	  op = field1 >> 6;
	  goto ccl_expr_self;

	case CCL_ExprSelfReg:	/* 00000OPERATION000RRRrrrXXXXX */
	  i = reg[RRR];
	  op = field1 >> 6;

	ccl_expr_self:
	  switch (op)
	    {
	    case CCL_PLUS: reg[rrr] += i; break;
	    case CCL_MINUS: reg[rrr] -= i; break;
	    case CCL_MUL: reg[rrr] *= i; break;
	    case CCL_DIV: reg[rrr] /= i; break;
	    case CCL_MOD: reg[rrr] %= i; break;
	    case CCL_AND: reg[rrr] &= i; break;
	    case CCL_OR: reg[rrr] |= i; break;
	    case CCL_XOR: reg[rrr] ^= i; break;
	    case CCL_LSH: reg[rrr] <<= i; break;
	    case CCL_RSH: reg[rrr] >>= i; break;
	    case CCL_LSH8: reg[rrr] <<= 8; reg[rrr] |= i; break;
	    case CCL_RSH8: reg[7] = reg[rrr] & 0xFF; reg[rrr] >>= 8; break;
	    case CCL_DIVMOD: reg[7] = reg[rrr] % i; reg[rrr] /= i; break;
	    case CCL_LS: reg[rrr] = reg[rrr] < i; break;
	    case CCL_GT: reg[rrr] = reg[rrr] > i; break;
	    case CCL_EQ: reg[rrr] = reg[rrr] == i; break;
	    case CCL_LE: reg[rrr] = reg[rrr] <= i; break;
	    case CCL_GE: reg[rrr] = reg[rrr] >= i; break;
	    case CCL_NE: reg[rrr] = reg[rrr] != i; break;
	    default: CCL_INVALID_CMD;
	    }
	  break;

	case CCL_SetExprConst:	/* 00000OPERATION000RRRrrrXXXXX */
	  i = reg[RRR];
	  j = XINT (ccl_prog[ic]);
	  op = field1 >> 6;
	  jump_address = ++ic;
	  goto ccl_set_expr;

	case CCL_SetExprReg:	/* 00000OPERATIONRrrRRRrrrXXXXX */
	  i = reg[RRR];
	  j = reg[Rrr];
	  op = field1 >> 6;
	  jump_address = ic;
	  goto ccl_set_expr;

	case CCL_ReadJumpCondExprConst: /* A--D--D--R--E--S--S-rrrXXXXX */
	  CCL_READ_CHAR (reg[rrr]);
	case CCL_JumpCondExprConst: /* A--D--D--R--E--S--S-rrrXXXXX */
	  i = reg[rrr];
	  op = XINT (ccl_prog[ic]);
	  jump_address = ic++ + ADDR;
	  j = XINT (ccl_prog[ic]);
	  ic++;
	  rrr = 7;
	  goto ccl_set_expr;

	case CCL_ReadJumpCondExprReg: /* A--D--D--R--E--S--S-rrrXXXXX */
	  CCL_READ_CHAR (reg[rrr]);
	case CCL_JumpCondExprReg:
	  i = reg[rrr];
	  op = XINT (ccl_prog[ic]);
	  jump_address = ic++ + ADDR;
	  j = reg[XINT (ccl_prog[ic])];
	  ic++;
	  rrr = 7;

	ccl_set_expr:
	  switch (op)
	    {
	    case CCL_PLUS: reg[rrr] = i + j; break;
	    case CCL_MINUS: reg[rrr] = i - j; break;
	    case CCL_MUL: reg[rrr] = i * j; break;
	    case CCL_DIV: reg[rrr] = i / j; break;
	    case CCL_MOD: reg[rrr] = i % j; break;
	    case CCL_AND: reg[rrr] = i & j; break;
	    case CCL_OR: reg[rrr] = i | j; break;
	    case CCL_XOR: reg[rrr] = i ^ j;; break;
	    case CCL_LSH: reg[rrr] = i << j; break;
	    case CCL_RSH: reg[rrr] = i >> j; break;
	    case CCL_LSH8: reg[rrr] = (i << 8) | j; break;
	    case CCL_RSH8: reg[rrr] = i >> 8; reg[7] = i & 0xFF; break;
	    case CCL_DIVMOD: reg[rrr] = i / j; reg[7] = i % j; break;
	    case CCL_LS: reg[rrr] = i < j; break;
	    case CCL_GT: reg[rrr] = i > j; break;
	    case CCL_EQ: reg[rrr] = i == j; break;
	    case CCL_LE: reg[rrr] = i <= j; break;
	    case CCL_GE: reg[rrr] = i >= j; break;
	    case CCL_NE: reg[rrr] = i != j; break;
	    case CCL_ENCODE_SJIS: ENCODE_SJIS (i, j, reg[rrr], reg[7]); break;
	    case CCL_DECODE_SJIS: DECODE_SJIS (i, j, reg[rrr], reg[7]); break;
	    default: CCL_INVALID_CMD;
	    }
	  code &= 0x1F;
	  if (code == CCL_WriteExprConst || code == CCL_WriteExprRegister)
	    {
	      i = reg[rrr];
	      CCL_WRITE_CHAR (i);
	    }
	  else if (!reg[rrr])
	    ic = jump_address;
	  break;

	case CCL_Extention:
	  switch (EXCMD)
	    {
	    case CCL_ReadMultibyteCharacter:
	      if (!src)
		CCL_INVALID_CMD;
	      do {
		if (src >= src_end)
		  goto ccl_read_multibyte_character_suspend;
	      
		i = *src++;
		if (i == LEADING_CODE_COMPOSITION)
		  {
		    if (src >= src_end)
		      goto ccl_read_multibyte_character_suspend;
		    if (*src == 0xFF)
		      {
			ccl->private_state = COMPOSING_WITH_RULE_HEAD;
			src++;
		      }
		    else
		      ccl->private_state = COMPOSING_NO_RULE_HEAD;
		  }
		if (ccl->private_state != 0)
		  {
		    /* composite character */
		    if (*src < 0xA0)
		      ccl->private_state = 0;
		    else
		      {
			if (i == 0xA0)
			  {
			    if (src >= src_end)
			      goto ccl_read_multibyte_character_suspend;
			    i = *src++ & 0x7F;
			  }
			else
			  i -= 0x20;

			if (COMPOSING_WITH_RULE_RULE == ccl->private_state)
			  {
			    ccl->private_state = COMPOSING_WITH_RULE_HEAD;
			    continue;
			  }
			else if (COMPOSING_WITH_RULE_HEAD == ccl->private_state)
			  ccl->private_state = COMPOSING_WITH_RULE_RULE;
		      }
		  }
		if (i < 0x80)
		  {
		    /* ASCII */
		    reg[rrr] = i;
		    reg[RRR] = CHARSET_ASCII;
		  }
		else if (i <= MAX_CHARSET_OFFICIAL_DIMENSION1)
		  {
		    if (src >= src_end)
		      goto ccl_read_multibyte_character_suspend;
		    reg[RRR] = i;
		    reg[rrr] = (*src++ & 0x7F);
		  }
		else if (i <= MAX_CHARSET_OFFICIAL_DIMENSION2)
		  {
		    if ((src + 1) >= src_end)
		      goto ccl_read_multibyte_character_suspend;
		    reg[RRR] = i;
		    i = (*src++ & 0x7F);
		    reg[rrr] = ((i << 7) | (*src & 0x7F));
		    src++;
		  }
		else if ((i == LEADING_CODE_PRIVATE_11) ||
			 (i == LEADING_CODE_PRIVATE_12))
		  {
		    if ((src + 1) >= src_end)
		      goto ccl_read_multibyte_character_suspend;
		    reg[RRR] = *src++;
		    reg[rrr] = (*src++ & 0x7F);
		  }
		else if ((i == LEADING_CODE_PRIVATE_21) ||
			 (i == LEADING_CODE_PRIVATE_22))
		  {
		    if ((src + 2) >= src_end)
		      goto ccl_read_multibyte_character_suspend;
		    reg[RRR] = *src++;
		    i = (*src++ & 0x7F);
		    reg[rrr] = ((i << 7) | (*src & 0x7F));
		    src++;
		  }
		else
		  {
		    /* INVALID CODE 
		       Returned charset is -1.*/
		    reg[RRR] = -1;
		  }
	      } while (0);
	      break;

	    ccl_read_multibyte_character_suspend:
	      src--;
	      if (ccl->last_block)
		{
		  ic = ccl->eof_ic;
		  goto ccl_finish;
		}
	      else
		CCL_SUSPEND (CCL_STAT_SUSPEND_BY_SRC);

	      break;

	    case CCL_WriteMultibyteCharacter:
	      i = reg[RRR]; /* charset */
	      if (i == CHARSET_ASCII)
		i = reg[rrr] & 0x7F;
	      else if (i == CHARSET_COMPOSITION)
		i = MAKE_COMPOSITE_CHAR (reg[rrr]);
	      else if (CHARSET_DIMENSION (i) == 1)
		i = ((i - 0x70) << 7) | (reg[rrr] & 0x7F);
	      else if (i < MIN_CHARSET_PRIVATE_DIMENSION2)
		i = ((i - 0x8F) << 14) | reg[rrr];
	      else
		i = ((i - 0xE0) << 14) | reg[rrr];

	      CCL_WRITE_CHAR (i);

	      break;

	    case CCL_UnifyCharacter:
	      i = reg[RRR]; /* charset */
	      if (i == CHARSET_ASCII)
		i = reg[rrr] & 0x7F;
	      else if (i == CHARSET_COMPOSITION)
		{
		  reg[RRR] = -1;
		  break;
		}
	      else if (CHARSET_DIMENSION (i) == 1)
		i = ((i - 0x70) << 7) | (reg[rrr] & 0x7F);
	      else if (i < MIN_CHARSET_PRIVATE_DIMENSION2)
		i = ((i - 0x8F) << 14) | (reg[rrr] & 0x3FFF);
	      else
		i = ((i - 0xE0) << 14) | (reg[rrr] & 0x3FFF);

	      op = unify_char (UNIFICATION_ID_TABLE (reg[Rrr]), i, -1, 0, 0);
	      SPLIT_CHAR (op, reg[RRR], i, j);
	      if (j != -1)
		i = (i << 7) | j;
	      
	      reg[rrr] = i;
	      break;

	    case CCL_UnifyCharacterConstTbl:
	      op = XINT (ccl_prog[ic]); /* table */
	      ic++;
	      i = reg[RRR]; /* charset */
	      if (i == CHARSET_ASCII)
		i = reg[rrr] & 0x7F;
	      else if (i == CHARSET_COMPOSITION)
		{
		  reg[RRR] = -1;
		  break;
		}
	      else if (CHARSET_DIMENSION (i) == 1)
		i = ((i - 0x70) << 7) | (reg[rrr] & 0x7F);
	      else if (i < MIN_CHARSET_PRIVATE_DIMENSION2)
		i = ((i - 0x8F) << 14) | (reg[rrr] & 0x3FFF);
	      else
		i = ((i - 0xE0) << 14) | (reg[rrr] & 0x3FFF);

	      op = unify_char (UNIFICATION_ID_TABLE (op), i, -1, 0, 0);
	      SPLIT_CHAR (op, reg[RRR], i, j);
	      if (j != -1)
		i = (i << 7) | j;
	      
	      reg[rrr] = i;
	      break;

	    case CCL_IterateMultipleMap:
	      {
		Lisp_Object table, content, attrib, value;
		int point, size, fin_ic;

		j = XINT (ccl_prog[ic++]); /* number of tables. */
		fin_ic = ic + j;
		op = reg[rrr];
		if ((j > reg[RRR]) && (j >= 0))
		  {
		    ic += reg[RRR];
		    i = reg[RRR];
		  }
		else
		  {
		    reg[RRR] = -1;
		    ic = fin_ic;
		    break;
		  }

		for (;i < j;i++)
		  {

		    size = XVECTOR (Vccl_translation_table_vector)->size;
		    point = XINT (ccl_prog[ic++]);
		    if (point >= size) continue;
		    table = XVECTOR (Vccl_translation_table_vector)->
		      contents[point];
		    if (!CONSP (table)) continue;
		    table = XCONS(table)->cdr;
		    if (!VECTORP (table)) continue;
		    size = XVECTOR (table)->size;
		    if (size <= 1) continue;
		    point = XUINT (XVECTOR (table)->contents[0]);
		    point = op - point + 1;
		    if (!((point >= 1) && (point < size))) continue;
		    content = XVECTOR (table)->contents[point];

		    if (NILP (content))
		      continue;
		    else if (NUMBERP (content))
		      {
			reg[RRR] = i;
			reg[rrr] = XUINT(content);
			break;
		      }
		    else if (EQ (content, Qt) || EQ (content, Qlambda))
		      {
			reg[RRR] = i;
			break;
		      }
		    else if (CONSP (content))
		      {
			attrib = XCONS (content)->car;
			value = XCONS (content)->cdr;
			if (!NUMBERP (attrib) || !NUMBERP (value))
			  continue;
			reg[RRR] = i;
			reg[rrr] = XUINT(value);
			break;
		      }
		  }
		if (i == j)
		  reg[RRR] = -1;
		ic = fin_ic;
	      }
	      break;
	      
	    case CCL_TranslateMultipleMap:
	      {
		Lisp_Object table, content, attrib, value;
		int point, size, table_vector_size;
		int skip_to_next, fin_ic;

		j = XINT (ccl_prog[ic++]); /* number of tables and separators. */
		fin_ic = ic + j;
		if ((j > reg[RRR]) && (j >= 0))
		  {
		    ic += reg[RRR];
		    i = reg[RRR];
		  }
		else
		  {
		    ic = fin_ic;
		    reg[RRR] = -1;
		    break;
		  }
		op = reg[rrr];
		reg[RRR] = -1;
		skip_to_next = 0;
		table_vector_size = XVECTOR (Vccl_translation_table_vector)->size;
		for (;i < j;i++)
		  {
		    point = XINT (ccl_prog[ic++]);
		    if (point == -1)
		      {
			skip_to_next = 0;
			continue;
		      }
		    if (skip_to_next) continue;
		    if (point >= table_vector_size) continue;
		    table = XVECTOR (Vccl_translation_table_vector)->
		      contents[point];
		    if (!CONSP (table)) continue;
		    table = XCONS (table)->cdr;
		    if (!VECTORP (table)) continue;
		    size = XVECTOR (table)->size;
		    if (size <= 1) continue;
		    point = XUINT (XVECTOR (table)->contents[0]);
		    point = op - point + 1;
		    if (!((point >= 1) && (point < size))) continue;
		    content = XVECTOR (table)->contents[point];

		    if (NILP (content))
		      continue;
		    else if (NUMBERP (content))
		      {
			op = XUINT (content);
			reg[RRR] = i;
			skip_to_next = 1;
		      }
		    else if (CONSP (content))
		      {
			attrib = XCONS (content)->car;
			value = XCONS (content)->cdr;
			if (!NUMBERP (attrib) || !NUMBERP (value))
			  continue;
			reg[RRR] = i;
			op = XUINT (value);
		      
		      }
		    else if (EQ (content, Qt))
		      {
			reg[RRR] = i;
			op = reg[rrr];
			skip_to_next = 1;
		      }
		    else if (EQ (content, Qlambda))
		      break;
		  }
		ic = fin_ic;
	      }
	      reg[rrr] = op;
	      break;

	    case CCL_TranslateSingleMap:
	      {
		Lisp_Object table, attrib, value, content;
		int size, point;
		j = XINT (ccl_prog[ic++]); /* table_id */
		op = reg[rrr];
		if (j >= XVECTOR (Vccl_translation_table_vector)->size)
		  {
		    reg[RRR] = -1;
		    break;
		  }
		table = XVECTOR (Vccl_translation_table_vector)->
		  contents[j];
		if (!CONSP (table))
		  {
		    reg[RRR] = -1;
		    break;
		  }
		table = XCONS(table)->cdr;
		if (!VECTORP (table))
		  {
		    reg[RRR] = -1;
		    break;
		  }
		size = XVECTOR (table)->size;
		point = XUINT (XVECTOR (table)->contents[0]);
		point = op - point + 1;
		reg[RRR] = 0;
		if ((size <= 1) ||
		    (!((point >= 1) && (point < size))))
		  reg[RRR] = -1;
		else
		  {
		    content = XVECTOR (table)->contents[point];
		    if (NILP (content))
		      reg[RRR] = -1;
		    else if (NUMBERP (content))
		      reg[rrr] = XUINT (content);
		    else if (EQ (content, Qt))
		      reg[RRR] = i;
		    else if (CONSP (content))
		      {
			attrib = XCONS (content)->car;
			value = XCONS (content)->cdr;
			if (!NUMBERP (attrib) || !NUMBERP (value))
			  continue;
			reg[rrr] = XUINT(value);
			break;
		      }
		    else
		      reg[RRR] = -1;
		  }
	      }
	      break;
	      
	    default:
	      CCL_INVALID_CMD;
	    }
	  break;

	default:
	  CCL_INVALID_CMD;
	}
    }

 ccl_error_handler:
  if (destination)
    {
      /* We can insert an error message only if DESTINATION is
         specified and we still have a room to store the message
         there.  */
      char msg[256];
      int msglen;

      switch (ccl->status)
	{
	case CCL_STAT_INVALID_CMD:
	  sprintf(msg, "\nCCL: Invalid command %x (ccl_code = %x) at %d.",
		  code & 0x1F, code, ic);
#ifdef CCL_DEBUG
	  {
	    int i = ccl_backtrace_idx - 1;
	    int j;

	    msglen = strlen (msg);
	    if (dst + msglen <= (dst_bytes ? dst_end : src))
	      {
		bcopy (msg, dst, msglen);
		dst += msglen;
	      }

	    for (j = 0; j < CCL_DEBUG_BACKTRACE_LEN; j++, i--)
	      {
		if (i < 0) i = CCL_DEBUG_BACKTRACE_LEN - 1;
		if (ccl_backtrace_table[i] == 0)
		  break;
		sprintf(msg, " %d", ccl_backtrace_table[i]);
		msglen = strlen (msg);
		if (dst + msglen > (dst_bytes ? dst_end : src))
		  break;
		bcopy (msg, dst, msglen);
		dst += msglen;
	      }
	  }
#endif
	  goto ccl_finish;

	case CCL_STAT_QUIT:
	  sprintf(msg, "\nCCL: Quited.");
	  break;

	default:
	  sprintf(msg, "\nCCL: Unknown error type (%d).", ccl->status);
	}

      msglen = strlen (msg);
      if (dst + msglen <= (dst_bytes ? dst_end : src))
	{
	  bcopy (msg, dst, msglen);
	  dst += msglen;
	}
    }

 ccl_finish:
  ccl->ic = ic;
  if (consumed) *consumed = src - source;
  return dst - destination;
}

/* Setup fields of the structure pointed by CCL appropriately for the
   execution of compiled CCL code in VEC (vector of integer).  */
void
setup_ccl_program (ccl, vec)
     struct ccl_program *ccl;
     Lisp_Object vec;
{
  int i;

  ccl->size = XVECTOR (vec)->size;
  ccl->prog = XVECTOR (vec)->contents;
  ccl->ic = CCL_HEADER_MAIN;
  ccl->eof_ic = XINT (XVECTOR (vec)->contents[CCL_HEADER_EOF]);
  ccl->buf_magnification = XINT (XVECTOR (vec)->contents[CCL_HEADER_BUF_MAG]);
  for (i = 0; i < 8; i++)
    ccl->reg[i] = 0;
  ccl->last_block = 0;
  ccl->private_state = 0;
  ccl->status = 0;
}

#ifdef emacs

DEFUN ("ccl-execute", Fccl_execute, Sccl_execute, 2, 2, 0,
  "Execute CCL-PROGRAM with registers initialized by REGISTERS.\n\
CCL-PROGRAM is a compiled code generated by `ccl-compile',\n\
 no I/O commands should appear in the CCL program.\n\
REGISTERS is a vector of [R0 R1 ... R7] where RN is an initial value\n\
 of Nth register.\n\
As side effect, each element of REGISTER holds the value of\n\
 corresponding register after the execution.")
  (ccl_prog, reg)
     Lisp_Object ccl_prog, reg;
{
  struct ccl_program ccl;
  int i;

  CHECK_VECTOR (ccl_prog, 0);
  CHECK_VECTOR (reg, 1);
  if (XVECTOR (reg)->size != 8)
    error ("Invalid length of vector REGISTERS");

  setup_ccl_program (&ccl, ccl_prog);
  for (i = 0; i < 8; i++)
    ccl.reg[i] = (INTEGERP (XVECTOR (reg)->contents[i])
		  ? XINT (XVECTOR (reg)->contents[i])
		  : 0);

  ccl_driver (&ccl, (char *)0, (char *)0, 0, 0, (int *)0);
  QUIT;
  if (ccl.status != CCL_STAT_SUCCESS)
    error ("Error in CCL program at %dth code", ccl.ic);

  for (i = 0; i < 8; i++)
    XSETINT (XVECTOR (reg)->contents[i], ccl.reg[i]);
  return Qnil;
}

DEFUN ("ccl-execute-on-string", Fccl_execute_on_string, Sccl_execute_on_string,
       3, 5, 0,
  "Execute CCL-PROGRAM with initial STATUS on STRING.\n\
CCL-PROGRAM is a compiled code generated by `ccl-compile'.\n\
Read buffer is set to STRING, and write buffer is allocated automatically.\n\
STATUS is a vector of [R0 R1 ... R7 IC], where\n\
 R0..R7 are initial values of corresponding registers,\n\
 IC is the instruction counter specifying from where to start the program.\n\
If R0..R7 are nil, they are initialized to 0.\n\
If IC is nil, it is initialized to head of the CCL program.\n\
\n\
If optional 4th arg CONTIN is non-nil, keep IC on read operation\n\
when read buffer is exausted, else, IC is always set to the end of\n\
CCL-PROGRAM on exit.\n\
\n\
It returns the contents of write buffer as a string,\n\
and as side effect, STATUS is updated.\n\
If the optional 5th arg UNIBYTE-P is non-nil, the returned string\n\
is a unibyte string.  By default it is a multibyte string.")
  (ccl_prog, status, str, contin, unibyte_p)
     Lisp_Object ccl_prog, status, str, contin, unibyte_p;
{
  Lisp_Object val;
  struct ccl_program ccl;
  int i, produced;
  int outbufsize;
  char *outbuf;
  struct gcpro gcpro1, gcpro2, gcpro3;

  CHECK_VECTOR (ccl_prog, 0);
  CHECK_VECTOR (status, 1);
  if (XVECTOR (status)->size != 9)
    error ("Invalid length of vector STATUS");
  CHECK_STRING (str, 2);
  GCPRO3 (ccl_prog, status, str);

  setup_ccl_program (&ccl, ccl_prog);
  for (i = 0; i < 8; i++)
    {
      if (NILP (XVECTOR (status)->contents[i]))
	XSETINT (XVECTOR (status)->contents[i], 0);
      if (INTEGERP (XVECTOR (status)->contents[i]))
	ccl.reg[i] = XINT (XVECTOR (status)->contents[i]);
    }
  if (INTEGERP (XVECTOR (status)->contents[i]))
    {
      i = XFASTINT (XVECTOR (status)->contents[8]);
      if (ccl.ic < i && i < ccl.size)
	ccl.ic = i;
    }
  outbufsize = STRING_BYTES (XSTRING (str)) * ccl.buf_magnification + 256;
  outbuf = (char *) xmalloc (outbufsize);
  if (!outbuf)
    error ("Not enough memory");
  ccl.last_block = NILP (contin);
  produced = ccl_driver (&ccl, XSTRING (str)->data, outbuf,
			 STRING_BYTES (XSTRING (str)), outbufsize, (int *)0);
  for (i = 0; i < 8; i++)
    XSET (XVECTOR (status)->contents[i], Lisp_Int, ccl.reg[i]);
  XSETINT (XVECTOR (status)->contents[8], ccl.ic);
  UNGCPRO;

  if (NILP (unibyte_p))
    val = make_string (outbuf, produced);
  else
    val = make_unibyte_string (outbuf, produced);
  free (outbuf);
  QUIT;
  if (ccl.status != CCL_STAT_SUCCESS
      && ccl.status != CCL_STAT_SUSPEND_BY_SRC
      && ccl.status != CCL_STAT_SUSPEND_BY_DST)
    error ("Error in CCL program at %dth code", ccl.ic);

  return val;
}

DEFUN ("register-ccl-program", Fregister_ccl_program, Sregister_ccl_program,
       2, 2, 0,
  "Register CCL program PROGRAM of NAME in `ccl-program-table'.\n\
PROGRAM should be a compiled code of CCL program, or nil.\n\
Return index number of the registered CCL program.")
  (name, ccl_prog)
     Lisp_Object name, ccl_prog;
{
  int len = XVECTOR (Vccl_program_table)->size;
  int i;

  CHECK_SYMBOL (name, 0);
  if (!NILP (ccl_prog))
    CHECK_VECTOR (ccl_prog, 1);
  
  for (i = 0; i < len; i++)
    {
      Lisp_Object slot = XVECTOR (Vccl_program_table)->contents[i];

      if (!CONSP (slot))
	break;

      if (EQ (name, XCONS (slot)->car))
	{
	  XCONS (slot)->cdr = ccl_prog;
	  return make_number (i);
	}
    }

  if (i == len)
    {
      Lisp_Object new_table = Fmake_vector (make_number (len * 2), Qnil);
      int j;

      for (j = 0; j < len; j++)
	XVECTOR (new_table)->contents[j]
	  = XVECTOR (Vccl_program_table)->contents[j];
      Vccl_program_table = new_table;
    }

  XVECTOR (Vccl_program_table)->contents[i] = Fcons (name, ccl_prog);
  return make_number (i);
}

/* register CCL translation table.
   CCL translation table consists of numbers and Qt and Qnil and Qlambda.
   The first element is start code point.
   The rest elements are translated numbers.
   Qt shows that an original number before translation.
   Qnil shows that an empty element.
   Qlambda makes translation stopped.
*/

DEFUN ("register-ccl-translation-table", Fregister_ccl_translation_table,
       Sregister_ccl_translation_table,
       2, 2, 0,
  "Register CCL translation table.\n\
TABLE should be a vector. SYMBOL is used for pointing the translation table out.\n\
Return index number of the registered translation table.")
  (symbol, table)
     Lisp_Object symbol, table;
{
  int len = XVECTOR (Vccl_translation_table_vector)->size;
  int i;
  Lisp_Object index;

  CHECK_SYMBOL (symbol, 0);
  CHECK_VECTOR (table, 1);
  
  for (i = 0; i < len; i++)
    {
      Lisp_Object slot = XVECTOR (Vccl_translation_table_vector)->contents[i];

      if (!CONSP (slot))
	break;

      if (EQ (symbol, XCONS (slot)->car))
	{
	  index = make_number (i);
	  XCONS (slot)->cdr = table;
	  Fput (symbol, Qccl_translation_table, table);
	  Fput (symbol, Qccl_translation_table_id, index);
	  return index;
	}
    }

  if (i == len)
    {
      Lisp_Object new_vector = Fmake_vector (make_number (len * 2), Qnil);
      int j;

      for (j = 0; j < len; j++)
	XVECTOR (new_vector)->contents[j]
	  = XVECTOR (Vccl_translation_table_vector)->contents[j];
      Vccl_translation_table_vector = new_vector;
    }

  index = make_number (i);
  Fput (symbol, Qccl_translation_table, table);
  Fput (symbol, Qccl_translation_table_id, index);
  XVECTOR (Vccl_translation_table_vector)->contents[i] = Fcons (symbol, table);
  return index;
}


void
syms_of_ccl ()
{
  staticpro (&Vccl_program_table);
  Vccl_program_table = Fmake_vector (make_number (32), Qnil);

  Qccl_program = intern("ccl-program");
  staticpro(&Qccl_program);

  Qccl_translation_table = intern ("ccl-translation-table");
  staticpro (&Qccl_translation_table);

  Qccl_translation_table_id = intern ("ccl-translation-table-id");
  staticpro (&Qccl_translation_table_id);

  DEFVAR_LISP ("ccl-translation-table-vector", &Vccl_translation_table_vector,
    "Where is stored translation tables for CCL program.\n\
Because CCL program can't access these tables except by the index of the vector.");
  Vccl_translation_table_vector = Fmake_vector (make_number (16), Qnil);

  DEFVAR_LISP ("font-ccl-encoder-alist", &Vfont_ccl_encoder_alist,
    "Alist of fontname patterns vs corresponding CCL program.\n\
Each element looks like (REGEXP . CCL-CODE),\n\
 where CCL-CODE is a compiled CCL program.\n\
When a font whose name matches REGEXP is used for displaying a character,\n\
 CCL-CODE is executed to calculate the code point in the font\n\
 from the charset number and position code(s) of the character which are set\n\
 in CCL registers R0, R1, and R2 before the execution.\n\
The code point in the font is set in CCL registers R1 and R2\n\
 when the execution terminated.\n\
If the font is single-byte font, the register R2 is not used.");
  Vfont_ccl_encoder_alist = Qnil;

  defsubr (&Sccl_execute);
  defsubr (&Sccl_execute_on_string);
  defsubr (&Sregister_ccl_program);
  defsubr (&Sregister_ccl_translation_table);
}

#endif  /* emacs */
