/* impl.c.mpm: GENERAL MPM SUPPORT
 *
 * $HopeName: MMsrc!mpm.c(MMdevel_restr.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#include "mpm.h"

SRCID(mpm, "$HopeName: MMsrc!mpm.c(MMdevel_restr.2) $");


/* MPMCheck -- test MPM assumptions */

Bool MPMCheck(void)
{
  CHECKL(sizeof(char) == 1);
  CHECKL(sizeof(Word) * CHAR_BIT == WORD_WIDTH);
  CHECKL(1uL << WORD_SHIFT == WORD_WIDTH);
  CHECKL(AlignCheck(ARCH_ALIGN));
  /* impl.c.mpm.check.ti: Check that trace ids will fit in the */
  /* TraceId type. */
  CHECKL(TRACE_MAX <= TraceIdNONE);
  CHECKL(TRACE_MAX <= UINT_MAX);
  /* impl.c.mpm.check.ts: Check that there are enough bits in */
  /* a TraceSet to store all possible trace ids. */
  CHECKL(sizeof(TraceSet) * CHAR_BIT >= TRACE_MAX);

  CHECKL((SizeAlignUp(0, 2048) == 0));
  CHECKL(!SizeIsAligned(64, (unsigned) -1));
  CHECKL(SizeIsAligned(0, 32));
  CHECKL((SizeAlignUp(1024, 16) == 1024));
  CHECKL(SizeIsAligned(SizeAlignUp(31051, 256), 256));
  CHECKL(SizeIsAligned(SizeAlignUp(31051, 512), 512));
  CHECKL(!SizeIsAligned(31051, 1024));
  CHECKL(!SizeIsP2(0));
  CHECKL(SizeIsP2(128));
  CHECKL(SizeLog2(1L) == 0);
  CHECKL(SizeLog2(256L) == 8);
  CHECKL(SizeLog2(65536L) == 16);
  CHECKL(SizeLog2(131072L) == 17);

  CHECKL(sizeof(char) == 1);
  CHECKL(sizeof(MPS_T_WORD)*8 == WORD_WIDTH);
  CHECKL(1u << WORD_SHIFT == WORD_WIDTH);
  CHECKL(AlignCheck(ARCH_ALIGN));

  return TRUE;  
}


/* AlignCheck -- check that an alignment is valid */

Bool AlignCheck(Align align)
{
  CHECKL(align > 0 && (align & (align - 1)) == 0);
  return TRUE;
}


/* WordIsAligned -- test whether a word is aligned */

Bool (WordIsAligned)(Word word, Align align)
{
  return (word & (align - 1)) == 0;
}


/* WordAlignUp -- round up a word to the nearest aligned value */

Word (WordAlignUp)(Word word, Align align)
{
  AVER(AlignCheck(align));
  return (word + align - 1) & ~(align - 1);
}


/* SizeIsP2 -- test whether a size is a power of two */

Bool SizeIsP2(Size size)
{
  return size > 0 && (size & (size - 1)) == 0;
}


/* SizeLog2 -- calculate the binary (base 2) log of a size */

Shift SizeLog2(Size size)
{
  Shift l = 0;
  AVER(SizeIsP2(size));

  while(size > 1) {
    ++l;
    size >>= 1;
  }

  return l;
}


/* AddrAdd -- add a size to an address */

Addr (AddrAdd)(Addr addr, Size size)
{
  Addr next = (Addr)((Word)addr + size);
  AVER(next >= addr);	/* overflow check */
  return next;
}


/* AddrOffset -- calculate the offset between two addresses */

Size (AddrOffset)(Addr base, Addr limit)
{
  AVER(base <= limit);
  return (Size)((Word)limit - (Word)base);
}
