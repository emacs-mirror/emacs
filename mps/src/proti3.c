/* impl.c.proti3: PROTECTION MUTATOR CONTEXT (Intel 386)
 *
 * $HopeName: !proti3.c(trunk.1) $
 * Copyright (C) 1998, 1999. Harlequin Group plc. All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * DESIGN
 *
 * .design: See design.mps.prot for the generic design of the interface
 * which is implemented in this module, including the contracts for the
 * functions.
 *
 * PURPOSE
 *
 * .purpose: This module implements the part of the protection module
 * that implements the MutatorFaultContext type.  
 *
 * REQUIREMENTS
 *
 * .requirements: Currently just limited support for stepping the 
 * sorts of instructions that the Dylan compiler might generate for 
 * table vector access - i.e. a restricted subset of MOV addressing 
 * modes. This avoids the need to scan entire weak tables at 
 * an inappropriate rank when a page fault occurs.
 *
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's 
 * Reference Manual
 *
 * .source.dylan: Dylan table code implementation. Especially the 
 * following HOPE units: 
 *  D-lib-dylan!table.dylan  (class <entry-vector>, slot entry-element)
 *  D-dfmc-harp-cg!harp-primitives.dylan (method op--repeated-slot-element)
 *  D-harp-pentium-harp!moves.dylan (pentium-template ld-index)
 *
 * ASSUMPTIONS
 *
 * .assume.null: It's always safe for Prot*StepInstruction to return 
 * ResUNIMPL. A null implementation of this module would be overly 
 * conservative but otherwise correct.
 *
 * .assume.want: The Dylan implementation is likely to access a
 * weak table vector using either MOV r/m32,r32 or MOV r32,r/m32
 * instructions, where the r/m32 operand will be of one of the forms 
 * disp8[reg], disp8[reg1][reg2], disp8[reg1][reg2*4] (see .source.dylan
 * and .source.i486)
 *
 * .assume.i3: Assume the following about the i386 environment:
 *   Steppable instructions (.assume.want) use the CS, DS & SS 
 *   segment registers only (see .source.i486 Table 2-3).
 *   The procesor runs in 32 bit mode.
 *   The CS, DS and SS segment registers all describe identical 32
 *   bit flat address spaces.
 * 
 */

#include "mpm.h"
#include "prmci3.h"

/* Decode an Intel x86 control byte into Hi, Medium & Low fields */
static void DecodeCB(unsigned int *hReturn, 
                     unsigned int *mReturn, 
                     unsigned int *lReturn, 
                     Byte op)
{
  /* see .source.i486 Figure 26-2 */
  unsigned int uop = (unsigned int)op;
  *lReturn = uop & 7;
  uop = uop >> 3;
  *mReturn = uop & 7;
  uop = uop >> 3;
  *hReturn = uop & 3;
}

/* Decode a Scale Index Base byte for an Intel x86 instruction */
static void DecodeSIB(unsigned int *sReturn, 
                      unsigned int *iReturn, 
                      unsigned int *bReturn, 
                      Byte op)
{
  DecodeCB(sReturn, iReturn, bReturn, op);
}

/* Decode a ModR/M byte for an Intel x86 instruction */
static void DecodeModRM(unsigned int *modReturn, 
                        unsigned int *rReturn, 
                        unsigned int *mReturn, 
                        Byte op)
{
  DecodeCB(modReturn, rReturn, mReturn, op);
}


/* Return the value of a machine register given a context and a 
 * register number
 */
static Word RegValue(MutatorFaultContext context, 
                     unsigned int regnum)
{
  MRef addr;

  addr = Prmci3AddressHoldingReg(context, regnum);
  return *addr;
}


/* Return a byte element of an instruction vector as a 
 * Word value, with sign extension
 */
static Word SignedInsElt(Byte insvec[], Count i)
{
  signed char eltb;

  eltb = ((signed char*)insvec)[i];
  return (Word)eltb;
}


/* If a MOV instruction is a sufficiently simple example of a
 * move between a register and memory (in either direction),
 * then find the register, the effective address and the size
 * of the instruction. The instruction is considered sufficiently 
 * simple if it uses a single byte displacement, a base register,
 * and either no index or a (possibly scaled) register.
 */
static Bool DecodeSimpleMov(unsigned int *regnumReturn, 
                            MRef *memReturn, 
                            Size *inslenReturn,
                            MutatorFaultContext context, 
                            Byte insvec[])
{
  unsigned int mod;
  unsigned int r;
  unsigned int m;

  DecodeModRM(&mod, &r, &m, insvec[1]);  /* .source.i486 Table 26-3 */
  if (1 == mod) {
    /* only know about single byte displacements, .assume.want */
    Word base;
    Word index;
    Word disp;

    if (4 == m) {
      /* There is an index */
      unsigned int s;
      unsigned int i;
      unsigned int b;

      DecodeSIB(&s, &i, &b, insvec[2]);  /* .source.i486 Table 26-3 */
      if (4 == i) 
        return FALSE; /* degenerate SIB form - unused by Dylan compiler */
      disp = SignedInsElt(insvec, 3);
      base = RegValue(context, b);
      index = RegValue(context, i) << s;
      *inslenReturn = 4;
    } else {
      /* MOV with reg1 & [reg2+byte] parameters */
      disp = SignedInsElt(insvec, 2);
      base = RegValue(context, m);
      index = 0;
      *inslenReturn = 3;
    }
    *regnumReturn = r;
    *memReturn = (MRef)(base + index + disp);  /* .assume.i3 */
    return TRUE;
  }

  return FALSE;
}

static Bool IsSimpleMov(Size *inslenReturn, 
                        MRef *srcReturn, 
                        MRef *destReturn,
                        MutatorFaultContext context)
{
  Byte *insvec;
  unsigned int regnum;
  MRef mem;
  MRef faultmem;

  Prmci3DecodeFaultContext(&faultmem, &insvec, context);

  /* .assume.want */
  /* .source.i486 Page 26-210 */
  if ((Byte)0x8b == insvec[0]) {
    /* This is an instruction of type  MOV reg, r/m32 */
    if (DecodeSimpleMov(&regnum, &mem, inslenReturn, context, insvec)) {
      AVER(faultmem == mem); /* Ensure computed address matches exception */
      *srcReturn = mem;
      *destReturn = Prmci3AddressHoldingReg(context, regnum);
      return TRUE;
    }
  } else if ((Byte)0x89 == insvec[0]) {
    /* This is an instruction of type  MOV r/m32, reg */
    if (DecodeSimpleMov(&regnum, &mem, inslenReturn, context, insvec)) {
      AVER(faultmem == mem); /* Ensure computed address matches exception */
      *destReturn = mem;
      *srcReturn = Prmci3AddressHoldingReg(context, regnum);
      return TRUE;
    }
  } 

  return FALSE;
}

Bool ProtCanStepInstruction(MutatorFaultContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  /* .assume.null */
  /* .assume.want */
  if (IsSimpleMov(&inslen, &src, &dest, context)) {
    return TRUE;
  }

  return FALSE;
}

Res ProtStepInstruction(MutatorFaultContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  /* .assume.null */
  /* .assume.want */
  if (IsSimpleMov(&inslen, &src, &dest, context)) {
    *dest = *src;
    Prmci3StepOverIns(context, inslen);
    return ResOK;
  }

  return ResUNIMPL;
}
