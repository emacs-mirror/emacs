/* impl.c.mpm: GENERAL MPM SUPPORT
 *
 * $HopeName: MMsrc!mpm.c(trunk.10) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MM developers.
 *
 * .purpose: Miscellaneous support for the implementation of the MPM
 * and pool classes.
 *
 * .sources: design.mps.writef
 */

#include "mpm.h"

SRCID(mpm, "$HopeName: MMsrc!mpm.c(trunk.10) $");


/* MPMCheck -- test MPM assumptions */

Bool MPMCheck(void)
{
  CHECKL(sizeof(char) == 1);
  CHECKL(sizeof(Word) * CHAR_BIT == MPS_WORD_WIDTH);
  CHECKL(1uL << MPS_WORD_SHIFT == MPS_WORD_WIDTH);
  CHECKL(AlignCheck(MPS_PF_ALIGN));
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
  /* .prime: 31051 is prime */
  CHECKL(SizeIsAligned(SizeAlignUp(31051, 256), 256));
  CHECKL(SizeIsAligned(SizeAlignUp(31051, 512), 512));
  CHECKL(!SizeIsAligned(31051, 1024));
  CHECKL(!SizeIsP2(0));
  CHECKL(SizeIsP2(128));
  CHECKL(SizeLog2(1L) == 0);
  CHECKL(SizeLog2(256L) == 8);
  CHECKL(SizeLog2(65536L) == 16);
  CHECKL(SizeLog2(131072L) == 17);

  /* .check.writef: We check that various types will fit in a Word; */
  /* See .writef.check.  Don't need to check WriteFS or WriteFF as they */
  /* should not be cast to Word. */
  CHECKL(sizeof(WriteFA) <= sizeof(Word));
  CHECKL(sizeof(WriteFP) <= sizeof(Word));
  CHECKL(sizeof(WriteFW) <= sizeof(Word)); /* Should be trivial*/
  CHECKL(sizeof(WriteFU) <= sizeof(Word));
  CHECKL(sizeof(WriteFB) <= sizeof(Word));
  CHECKL(sizeof(WriteFC) <= sizeof(Word));

  return TRUE;  
}


/* BoolCheck -- check that a boolean is valid */

Bool BoolCheck(Bool b)
{
  AVER(b == TRUE || b == FALSE);
  return TRUE;
}


/* FunCheck -- check that a function pointer is valid */

Bool FunCheck(Fun f)
{
  AVER(f != NULL);
  /* Could assert various platform-specific things here. */
  return TRUE;
}


/* AttrCheck -- check that a set of pool attributes are valid */

Bool AttrCheck(Attr attr)
{
  AVER(((attr) & ~AttrMASK) == 0);
  /* Could check for legal combinations of attributes. */
  return TRUE;
}


/* .rootvarcheck: RootVarCheck -- check a Root union discriminator */

Bool RootVarCheck(RootVar rootVar)
{
  AVER(rootVar == RootTABLE || rootVar == RootFUN || rootVar == RootFMT ||
    rootVar == RootREG);
  return(TRUE);
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


/* Logarithms */

Shift SizeFloorLog2(Size size)
{
  Shift l = 0;

  AVER(size != 0);

  while(size > 1) {
    ++l;
    size >>= 1;
  }

  return l;
}

Shift SizeLog2(Size size)
{
  AVER(SizeIsP2(size));

  return SizeFloorLog2(size);
}


/* AddrAdd -- add a size to an address */

Addr (AddrAdd)(Addr addr, Size size)
{
  Addr next;
  AVER(addr != (Addr)0);
  next = AddrAdd(addr, size);
  AVER(next >= addr);   /* overflow check */
  return next;
}


/* AddrSub -- subtract a size from an address */

Addr (AddrSub)(Addr addr, Size size)
{
  Addr next;
  AVER(addr != (Addr)0);
  next = AddrSub(addr, size);
  AVER(next <= addr);   /* overflow check */
  return next;
}


/* AddrOffset -- calculate the offset between two addresses */

Size (AddrOffset)(Addr base, Addr limit)
{
  AVER(base != 0);
  AVER(limit != 0);
  AVER(base <= limit);
  return AddrOffset(base, limit);
}


/* PointerAdd -- add a size to a pointer */

Pointer (PointerAdd)(Pointer p, Size s)
{
  Pointer next;
  AVER(p != NULL);
  next = PointerAdd(p, s);
  AVER(next >= p);   /* overflow check */
  return next;
}


/* PointerSub -- subtract a size from a pointer */

Pointer (PointerSub)(Pointer p, Size s)
{
  Pointer next;
  AVER(p != NULL);
  next = PointerSub(p, s);
  AVER(next <= p);   /* overflow check */
  return next;
}


/* PointerOffset -- calculate the offset between two addresses */

Size (PointerOffset)(Pointer base, Pointer limit)
{
  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base <= limit);
  return PointerOffset(base, limit);
}


/* WriteWord -- output a textual representation of a word to a stream */
/* as an unsigned value in the given base (2-16), */
/* padded to the given width */

static Res WriteWord(mps_lib_FILE *stream, Word w, unsigned base, 
  unsigned width)
{
  static const char digit[16] = "0123456789ABCDEF";
  static const char pad = '0'; /* padding character */
  char buf[MPS_WORD_WIDTH + 1]; /* enough for binary, */
                                /* plus one for terminator */
  unsigned i;
  int r;

  AVER(stream != NULL);
  AVER(2 <= base && base <= 16);
  AVER(width <= MPS_WORD_WIDTH);
  
  /* Add digits to the buffer starting at the right-hand end, so that */
  /* the buffer forms a string representing the number.  A do...while */
  /* loop is used to ensure that at least one digit (zero) is written */
  /* when the number is zero. */
  i = MPS_WORD_WIDTH;
  buf[i] = '\0';
  do {
    --i;
    buf[i] = digit[w % base];
    w /= base;
  } while(w > 0);

  /* If the number is not as wide as the requested field, pad out the */
  /* buffer with zeros. */
  while(i > MPS_WORD_WIDTH - width) {
    --i;
    buf[i] = pad;
  }

  r = mps_lib_fputs(&buf[i], stream);
  if(r == mps_lib_EOF)
    return ResIO;

  return ResOK;
}


/* WriteF -- write formatted output
 *
 * .writef.des: See design.mps.writef, also design.mps.lib
 *
 * .writef.p: There is an assumption that void * fits in Word in
 * the case of $P, and unsigned long for $U and $B.  This is checked in
 * MPMCheck.
 *
 * .writef.div: Although MPS_WORD_WIDTH/4 appears three times, there
 * are effectively three separate decisions to format at this width.
 *
 * .writef.check: See .check.writef
 */

Res WriteF(mps_lib_FILE *stream, ...)
{
  const char *format;
  int r, i;
  Res res;
  va_list args;

  AVER(stream != NULL);
  
  va_start(args, stream);
  
  for(;;) {
    format = va_arg(args, const char *);
    if(format == NULL)
      break;

    while(*format != '\0') {
      if(*format != '$') {
        r = mps_lib_fputc(*format, stream); /* Could be more efficient */
        if(r == mps_lib_EOF)
          return ResIO;
      } else {
        ++format;
        AVER(*format != '\0');

        switch(*format) {
          case 'A': {			/* address */
            WriteFA addr = va_arg(args, WriteFA);
            res = WriteWord(stream, (Word)addr, 16, 
                            (sizeof(WriteFA) * CHAR_BIT + 3) / 4);
            if(res != ResOK) return res;
          } break;

          case 'P': {			/* pointer, see .writef.p */
            WriteFP p = va_arg(args, WriteFP);
            res = WriteWord(stream, (Word)p, 16, 
                            (sizeof(WriteFP) * CHAR_BIT + 3)/ 4);
            if(res != ResOK) return res;
          } break;

          case 'F': {                   /* function */
            WriteFF f = va_arg(args, WriteFF);
            Byte *b = (Byte *)&f;
            for(i=0; i < sizeof(WriteFF); i++) {
              res = WriteWord(stream, (Word)(b[i]), 16, 
                              (CHAR_BIT + 3) / 4);
              if(res != ResOK) return res;
            }
          } break;
            
          case 'S': {			/* string */
            WriteFS s = va_arg(args, WriteFS);
            r = mps_lib_fputs((char *)s, stream);
            if(r == mps_lib_EOF)
              return ResIO;
          } break;
        
          case 'C': {			/* character */
            WriteFC c = va_arg(args, WriteFC); /* promoted */
            r = mps_lib_fputc((int)c, stream);
            if(r == mps_lib_EOF)
              return ResIO;
          } break;
        
          case 'W': {			/* word */
            WriteFW w = va_arg(args, WriteFW);
            res = WriteWord(stream, (Word)w, 16,
                            (sizeof(WriteFW) * CHAR_BIT + 3) / 4);
            if(res != ResOK) return res;
          } break;

          case 'U': {			/* decimal, see .writef.p */
            WriteFU u = va_arg(args, WriteFU);
            res = WriteWord(stream, (Word)u, 10, 0);
            if(res != ResOK) return res;
          } break;

          case 'B': {			/* binary, see .writef.p */
            WriteFB b = va_arg(args, WriteFB);
            res = WriteWord(stream, (Word)b, 2, sizeof(WriteFB) * CHAR_BIT);
            if(res != ResOK) return res;
          } break;
        
          case '$': {			/* dollar char */
            r = mps_lib_fputc('$', stream);
            if(r == mps_lib_EOF)
              return ResIO;
          } break;

          default:
          NOTREACHED;
        }
      }

      ++format;
    }
  }
  
  va_end(args);
  
  return ResOK;
}
