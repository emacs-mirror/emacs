/* impl.c.mpm: GENERAL MPM SUPPORT
 *
 * $HopeName: MMsrc!mpm.c(trunk.29) $
 * Copyright (C) 1996 Harlequin Limited.  All rights reserved.
 *
 * .purpose: Miscellaneous support for the implementation of the MPM
 * and pool classes.
 *
 * .sources: design.mps.writef
 */

#include "mpm.h"
#include <stdarg.h>
/* Get some floating constants for WriteDouble */
#include <float.h>


SRCID(mpm, "$HopeName: MMsrc!mpm.c(trunk.29) $");


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
  /* .check.write.double: See .write.double.check */
  {
    int e, DBL_EXP_DIG = 1;
    for (e = DBL_MAX_10_EXP; e > 0; e /= 10)
      DBL_EXP_DIG++;
    CHECKL(DBL_EXP_DIG < DBL_DIG);
    CHECKL(-(DBL_MIN_10_EXP) <= DBL_MAX_10_EXP);
  }

  return TRUE;  
}


/* BoolCheck -- check that a boolean is valid
 *
 * See design.mps.type.bool.check.
 * We expect this to be inlined (by the macro in impl.h.mpm).
 * See design.mps.type.bool.check.inline.
 */

Bool (BoolCheck)(Bool b)
{
  CHECKL(BoolCheck(b));
  return TRUE;
}


/* FunCheck -- check that a function pointer is valid */

Bool FunCheck(Fun f)
{
  CHECKL(f != NULL);
  /* Could assert various platform-specific things here. */
  return TRUE;
}


/* ShiftCheck -- check that a shift is valid */

Bool ShiftCheck(Shift shift)
{
  CHECKL(shift < MPS_WORD_WIDTH);         /* standard.ansic 6.3.7 */
  return TRUE;
}


/* AttrCheck -- check that a set of pool attributes are valid */

Bool AttrCheck(Attr attr)
{
  CHECKL((attr & ~AttrMASK) == 0);
  /* Could check for legal combinations of attributes. */
  return TRUE;
}


/* AlignCheck -- check that an alignment is valid */

Bool AlignCheck(Align align)
{
  CHECKL(align > 0 && (align & (align - 1)) == 0);
  /* .check.unused: Check methods for signatureless types don't use */
  /* their argument in hot varieties, so UNUSED is needed. */
  UNUSED(align);
  return TRUE;
}


/* WordIsAligned -- test whether a word is aligned */

Bool (WordIsAligned)(Word word, Align align)
{
  AVER(AlignCheck(align));
  return WordIsAligned(word, align);
}


/* WordAlignUp -- round a word up to the nearest aligned value */

Word (WordAlignUp)(Word word, Align align)
{
  AVER(AlignCheck(align));
  return WordAlignUp(word, align);
}

/* WordRoundUp -- round word up to round.
 *
 * .wordroundup.arg.word: the quantity to be rounded.
 * .wordroundup.arg.round: The modulus to round to.  Not necessarily
 *   an alignment (ie not a power of two).
 *
 * .wordroundup.result:
 * Let m be congruent to 0 mod r (m == 0(r)),
 *   and let m be the least m >= w.
 * If w+r-1 (!) is representible in Word then result is m.
 * Otherwise result is 0.  Wittily.
 * (NB result may be 0 even if m is representible)
 */
Word (WordRoundUp)(Word word, Size round)
{
  AVER(round > 0);
  return WordRoundUp(word, round);
}


/* WordAlignUp -- round a word down to the nearest aligned value */

Word (WordAlignDown)(Word word, Align alignment)
{
  AVER(AlignCheck(alignment));
  return WordAlignDown(word, alignment);
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


/* AddrAlignDown -- round a word down to the nearest aligned value */

Addr (AddrAlignDown)(Addr addr, Align alignment)
{
  AVER(AlignCheck(alignment));
  return AddrAlignDown(addr, alignment);
}


/* PointerAdd -- add a size to a pointer */

void *(PointerAdd)(void *p, size_t s)
{
  void *next;
  AVER(p != NULL);
  next = PointerAdd(p, s);
  AVER(next >= p);   /* overflow check */
  return next;
}


/* PointerSub -- subtract a size from a pointer */

void *(PointerSub)(void *p, size_t s)
{
  void *next;
  AVER(p != NULL);
  next = PointerSub(p, s);
  AVER(next <= p);   /* overflow check */
  return next;
}


/* PointerOffset -- calculate the offset between two addresses */

size_t (PointerOffset)(void *base, void *limit)
{
  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base <= limit);
  return PointerOffset(base, limit);
}


/* PointerAlignUp -- align a pointer up */

extern void *(PointerAlignUp)(void *p, size_t align)
{
  AVER(p != NULL);
  AVER(AlignCheck(align));

  /* uses macro defined in impl.h.mpm */
  return PointerAlignUp(p, align);
}


/* ResIsAllocFailure 
 *
 * Test whether a result code is in the set of allocation failure codes.
 */

Bool ResIsAllocFailure(Res res)
{
  return res == ResMEMORY || res == ResRESOURCE || res == ResCOMMIT_LIMIT;
}


/* WriteWord -- output a textual representation of a word to a stream
 *
 * as an unsigned value in the given base (2-16),
 * padded to the given width.
 */

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


/* WriteDouble -- write a double float to a stream
 *
 * Cf.: Guy L. Steele, Jr. and Jon L. White, "How to print
 * floating-point numbers accurately", ACM SIGPLAN Notices, Vol. 25,
 * No. 6 (Jun. 1990), Pages 112-126
 *
 * .write.double.limitation: Only the "simple" printer is implemented
 * here.
 *
 * .write.double.check: There being no DBL_EXP_DIG, we assume that it
 * is less than DBL_DIG.
 */

static Res WriteDouble(mps_lib_FILE *stream, double d) 
{
  double F = d;
  int E = 0, i, x = 0;
  /* Largest exponent that will print in %f style.  Larger will use %e
     style.  DBL_DIG is chosen for use of doubles as extra-large
     intergers. */
  int expmax = DBL_DIG;
  /* Smallest exponent that will print in %f style.  Smaller will use
     %e style.  -4 is chosen because it is the %g default. */
  int expmin = -4;
  /* Epsilon defines how many digits will be printed.  Using
     DBL_EPSILON prints all the significant digits.  To print fewer
     digits, set epsilon to 10 ^ - N, where N is the desired number of
     digits */
  double epsilon = DBL_EPSILON / 2;
  char digits[] = "0123456789";
  /* sign, DBL_DIG, '0.', 'e', '+/-', log10(DBL_MAX_10_EXP),
     terminator.  See .write.double.check */
  char buf[1+DBL_DIG+2+1+1+DBL_DIG+1];
  int j = 0;
  
  if (F == 0.0) {
    if(mps_lib_fputs("0", stream) == mps_lib_EOF)
      return ResIO;
    return ResOK;
  }

  if (F < 0) {
    buf[j] = '-';
    j++;
    F = - F;
  }
  
  /* This scaling operation could introduce rounding errors */
  for ( ; F >= 1.0 ; F /= 10.0) {
    E++;
    if (E > DBL_MAX_10_EXP) {
      if(mps_lib_fputs("Infinity", stream) == mps_lib_EOF)
        return ResIO;
      return ResOK;
    }
  }
  for ( ; F < 0.1; F *= 10)
    E--;
    
  /* See if %e notation is required */
  if (E > expmax || E <= expmin) {
    x = E - 1;
    E = 1;
  }

  /* Insert leading 0's */
  if (E <= 0) {
    buf[j] = '0';
    j++;
  }
  if (E < 0) {
    buf[j] = '.';
    j++;
  }
  for (i = -E; i > 0; i--) {
    buf[j] = '0';
    j++;
  }

  /* Convert the fraction to base 10, inserting a decimal according to
     the exponent.  This is Steele and White's FP3 algorithm. */
  do {
    int U;
    
    if (E == 0) {
      buf[j] = '.';
      j++;
    }
    F *= 10.0;
    U = (int)F;
    F = F - U;
    epsilon *= 10.0;
    E--;
    if (F < epsilon || F > 1.0 - epsilon) {
      if (F < 0.5)
        buf[j] = digits[U];
      else
        buf[j] = digits[U + 1];
      j++;
      break;
    }
    buf[j] = digits[U];
    j++;
  } while (1);
  
  /* Insert trailing 0's */
  for (i = E; i > 0; i--) {
    buf[j] = '0';
    j++;
  }

  /* If %e notation is selected, append the exponent indicator and
     sign  */
  if (x != 0) {
    buf[j] = 'e';
    j++;
    if (x < 0) {
      buf[j] = '-';
      j++;
      x = - x;
    }
    else {
      buf[j] = '+';
      j++;
    }

    /* Format the exponent to at least two digits */
    for (i = 100; i <= x; )
      i *= 10;
    i /= 10;
    do {
      buf[j] = digits[x / i];
      j++;
      x %= i;
      i /= 10;
    } while (i > 0);
  }
  buf[j] = '\0';                /* arnold */
  
  if(mps_lib_fputs(buf, stream) == mps_lib_EOF)
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
  int r;
  size_t i;
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
          case 'A': {                   /* address */
            WriteFA addr = va_arg(args, WriteFA);
            res = WriteWord(stream, (Word)addr, 16, 
                            (sizeof(WriteFA) * CHAR_BIT + 3) / 4);
            if(res != ResOK) return res;
          } break;

          case 'P': {                   /* pointer, see .writef.p */
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
            
          case 'S': {                   /* string */
            WriteFS s = va_arg(args, WriteFS);
            r = mps_lib_fputs((const char *)s, stream);
            if(r == mps_lib_EOF)
              return ResIO;
          } break;
        
          case 'C': {                   /* character */
            WriteFC c = va_arg(args, WriteFC); /* promoted */
            r = mps_lib_fputc((int)c, stream);
            if(r == mps_lib_EOF)
              return ResIO;
          } break;
        
          case 'W': {                   /* word */
            WriteFW w = va_arg(args, WriteFW);
            res = WriteWord(stream, (Word)w, 16,
                            (sizeof(WriteFW) * CHAR_BIT + 3) / 4);
            if(res != ResOK) return res;
          } break;

          case 'U': {                   /* decimal, see .writef.p */
            WriteFU u = va_arg(args, WriteFU);
            res = WriteWord(stream, (Word)u, 10, 0);
            if(res != ResOK) return res;
          } break;

          case 'B': {                   /* binary, see .writef.p */
            WriteFB b = va_arg(args, WriteFB);
            res = WriteWord(stream, (Word)b, 2, sizeof(WriteFB) * CHAR_BIT);
            if(res != ResOK) return res;
          } break;
        
          case '$': {                   /* dollar char */
            r = mps_lib_fputc('$', stream);
            if(r == mps_lib_EOF)
              return ResIO;
          } break;

          case 'D': {                   /* double */
            WriteFD d = va_arg(args, WriteFD);
            res = WriteDouble(stream, d);
            if (res != ResOK) return res;
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


/* StringLength -- Slow substitute for strlen */

size_t StringLength(const char *s)
{
  size_t i;

  AVER(s != NULL);

  for(i = 0; s[i] != '\0'; i++) 
    NOOP;
  return(i);
}
