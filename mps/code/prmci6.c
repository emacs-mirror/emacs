/* prmci6.c: MUTATOR CONTEXT (x64)
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See <design/prmc> for the generic design of the interface
 * which is implemented in this module, including the contracts for the
 * functions.
 *
 * .purpose: Implement the mutator context module. <design/prmc>.
 *
 *
 * SOURCES
 *
 * .source.amd64: AMD64 Architecture Programmer's Manual Volume 3:
 * General-Purpose and System Instructions
 * <https://www.amd.com/system/files/TechDocs/24594.pdf>
 *
 *
 * ASSUMPTIONS
 *
 * .assume.null: It's always safe for MutatorContextCanStepInstruction
 * to return FALSE. A null implementation of this module would be
 * overly conservative but otherwise correct.
 *
 */

#include "mpm.h"
#include "prmci6.h"

SRCID(prmci6, "$Id$");

#if !defined(MPS_ARCH_I6)
#error "prmci6.c is specific to MPS_ARCH_I6"
#endif

/* DecodeCB -- Decode an Intel x86 control byte into Hi, Medium & Low
   fields */

static void DecodeCB(size_t *hReturn, size_t *mReturn, size_t *lReturn,
                     Byte op)
{
  *lReturn = op & 7;
  *mReturn = (op >> 3) & 7;
  *hReturn = (op >> 6) & 3;
}


/* DecodeSIB -- Decode a Scale Index Base byte for an Intel x86
   instruction */

static void DecodeSIB(size_t *sReturn, size_t *iReturn, size_t *bReturn,
                      Byte op)
{
  DecodeCB(sReturn, iReturn, bReturn, op);
}


/* DecodeModRM -- Decode a ModR/M byte for an Intel x86 instruction */

static void DecodeModRM(size_t *modReturn, size_t *rReturn,
                        size_t *mReturn, Byte op)
{
  DecodeCB(modReturn, rReturn, mReturn, op);
}


/* RegValue -- Return the value of a machine register from a context */

static Word RegValue(MutatorContext context, size_t regnum)
{
  MRef addr;

  addr = Prmci6AddressHoldingReg(context, regnum);
  return *addr;
}


/* Return a byte element of an instruction vector as a
 * Word value, with sign extension
 */
static Word SignedInsElt(Byte insvec[], Count i)
{
  signed char eltb;

  eltb = ((signed char *)insvec)[i];
  return (Word)eltb;
}

static signed DecodeDisp32(Byte *bytes)
{
  unsigned r = 0;
  size_t i;
  for (i = 0; i < 4; i++) r |= bytes[i] << (i * 8);
  return r;
}

static Word RexR(Byte rex) { return (rex >> 2) & 1; }
static Word RexB(Byte rex) { return (rex >> 0) & 1; }
static Word RexX(Byte rex) { return (rex >> 1) & 1; }

static Bool DecodeSimpleMov(size_t *regnumReturn, MRef *memReturn,
                            Size *inslenReturn, MutatorContext context,
                            Byte insvec[])
{
  Byte rex = insvec[0];
  Byte modRM = insvec[2];
  Word base, disp = 0, scaled_index = 0;
  size_t mod, reg, rm;
  DecodeModRM(&mod, &reg, &rm, modRM);
  switch (mod) {
    case 0:
      switch (rm) {
        default:
          *inslenReturn = 3;
          base = RegValue(context, rm | (RexB(rex) << 3));
          goto done;

        case 4:
          *inslenReturn = 4;
          goto decode_sib;

        case 5: {
          Word rip = (Word)insvec;
          *inslenReturn = 7;
          base = rip + *inslenReturn;
          disp = DecodeDisp32(insvec + 3);
          goto done;
        }
      }

    case 1:
      switch (rm) {
        default:
          *inslenReturn = 4;
          disp = SignedInsElt(insvec, 3);
          base = RegValue(context, rm | (RexB(rex) << 3));
          goto done;

        case 4:
          *inslenReturn = 5;
          disp = SignedInsElt(insvec, 4);
          goto decode_sib;
      }

    case 2:
      switch (rm) {
        default:
          *inslenReturn = 7;
          disp = DecodeDisp32(insvec + 3);
          base = RegValue(context, rm | (RexB(rex) << 3));
          goto done;

        case 4:
          *inslenReturn = 8;
          disp = DecodeDisp32(insvec + 4);
          goto decode_sib;
      }

    case 3:
      return FALSE;

    default:
      NOTREACHED;
  }
  NOTREACHED;

decode_sib: {
  size_t s, i, b;
  DecodeSIB(&s, &i, &b, insvec[3]);
  base = RegValue(context, b | (RexB(rex) << 3));
  if (i == 4 && !RexX(rex))
    scaled_index = 0;
  else
    scaled_index = (RegValue(context, i | RexX(rex) << 3) << s);
}

done:
  *memReturn = (MRef)(base + disp + scaled_index);
  *regnumReturn = reg | (RexR(rex) << 3);
  return TRUE;
}

static Bool IsRexPrefix(Byte b) { return (b >> 4) == 0x4; }

static Bool IsSimpleMov(Size *inslenReturn, MRef *srcReturn,
                        MRef *destReturn, MutatorContext context)
{
  Byte *insvec;
  size_t regnum;
  MRef mem;
  MRef faultmem;

  Prmci6DecodeFaultContext(&faultmem, &insvec, context);

  if (IsRexPrefix(insvec[0])) {
    switch (insvec[1]) {
      case 0x8b: /* MOV r64, r/m64 */
        if (DecodeSimpleMov(&regnum, &mem, inslenReturn, context,
                            insvec)) {
          AVER(faultmem == mem); /* Ensure computed address
                                    matches exception */
          *srcReturn = mem;
          *destReturn = Prmci6AddressHoldingReg(context, regnum);
          return TRUE;
        } else
          return FALSE;

      case 0x89: /* MOV r/m64, r64 */
        if (DecodeSimpleMov(&regnum, &mem, inslenReturn, context,
                            insvec)) {
          AVER(faultmem == mem); /* Ensure computed address
                                    matches exception */
          *destReturn = mem;
          *srcReturn = Prmci6AddressHoldingReg(context, regnum);
          return TRUE;
        } else
          return FALSE;

      default:
        return FALSE;
    }
  }

  return FALSE;
}

Bool MutatorContextCanStepInstruction(MutatorContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  AVERT(MutatorContext, context);

  /* .assume.null */
  if (IsSimpleMov(&inslen, &src, &dest, context)) {
    return TRUE;
  }

  return FALSE;
}


Res MutatorContextStepInstruction(MutatorContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  AVERT(MutatorContext, context);

  /* .assume.null */
  if (IsSimpleMov(&inslen, &src, &dest, context)) {
    *dest = *src;
    Prmci6StepOverIns(context, inslen);
    return ResOK;
  }

  return ResUNIMPL;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
