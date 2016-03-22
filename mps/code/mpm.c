/* mpm.c: GENERAL MPM SUPPORT
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Miscellaneous support for the implementation of the MPM
 * and pool classes.
 *
 * .sources: <design/writef/> */

#include "check.h"
#include "mpm.h"
#include "vm.h"

#include <stdarg.h>
/* Get some floating constants for WriteDouble */
#include <float.h>
#include <limits.h>

SRCID(mpm, "$Id$");


#if defined(AVER_AND_CHECK)


/* CheckLevel -- Control check level 
 *
 * This controls the behaviour of Check methods (see check.h).
 */

#ifdef CHECKLEVEL_DYNAMIC
unsigned CheckLevel = CHECKLEVEL_DYNAMIC;
#endif


/* MPMCheck -- test MPM assumptions */

Bool MPMCheck(void)
{
  CHECKL(sizeof(Word) * CHAR_BIT == MPS_WORD_WIDTH);
  CHECKL((Word)1 << MPS_WORD_SHIFT == MPS_WORD_WIDTH);
  CHECKL(AlignCheck(MPS_PF_ALIGN));
  /* Check that trace ids will fit in the TraceId type. */
  CHECKL(TraceLIMIT <= UINT_MAX);
  /* Check that there are enough bits in */
  /* a TraceSet to store all possible trace ids. */
  CHECKL(sizeof(TraceSet) * CHAR_BIT >= TraceLIMIT);

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
  CHECKL(SizeLog2((Size)1) == 0);
  CHECKL(SizeLog2((Size)256) == 8);
  CHECKL(SizeLog2((Size)65536) == 16);
  CHECKL(SizeLog2((Size)131072) == 17);

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

  /* The granularity of memory mapping must be a multiple of the
   * granularity of protection (or we might not be able to protect an
   * arena grain). */
  CHECKL(PageSize() % ProtGranularity() == 0);

  /* StackProbe mustn't skip over the stack guard page. See
   * <design/sp/#sol.depth.constraint>. */
  CHECKL(StackProbeDEPTH * sizeof(Word) < PageSize());

  /* Check these values will fit in their bitfield. */
  CHECKL(WB_DEFER_INIT  <= ((1ul << WB_DEFER_BITS) - 1));
  CHECKL(WB_DEFER_DELAY <= ((1ul << WB_DEFER_BITS) - 1));
  CHECKL(WB_DEFER_HIT   <= ((1ul << WB_DEFER_BITS) - 1));

  return TRUE;
}


/* FunCheck -- check that a function pointer is valid */

Bool FunCheck(Fun f)
{
  CHECKL(f != NULL);
  /* Could assert various platform-specific things here. */
  UNUSED(f); /* see .check.unused */
  return TRUE;
}


/* ShiftCheck -- check that a shift is valid */

Bool ShiftCheck(Shift shift)
{
  CHECKL(shift < MPS_WORD_WIDTH);         /* standard.ansic 6.3.7 */
  UNUSED(shift); /* see .check.unused */
  return TRUE;
}


/* AttrCheck -- check that a set of pool attributes are valid */

Bool AttrCheck(Attr attr)
{
  CHECKL((attr & ~AttrMASK) == 0);
  /* Could check for legal combinations of attributes. */
  UNUSED(attr); /* see .check.unused */
  return TRUE;
}


/* AlignCheck -- check that an alignment is valid */

Bool AlignCheck(Align align)
{
  CHECKL(align > 0);
  CHECKL((align & (align - 1)) == 0);
  /* .check.unused: Check methods for signatureless types don't use */
  /* their argument in hot varieties, so UNUSED is needed. */
  UNUSED(align);
  return TRUE;
}


/* AccessSetCheck -- check that an access set is valid */

Bool AccessSetCheck(AccessSet mode)
{
  CHECKL(mode < ((ULongest)1 << AccessLIMIT));
  UNUSED(mode); /* see .check.unused */
  return TRUE;
}


#endif /* defined(AVER_AND_CHECK) */


/* WordIsAligned -- test whether a word is aligned */

Bool (WordIsAligned)(Word word, Align align)
{
  AVERT(Align, align);
  return WordIsAligned(word, align);
}


/* WordAlignUp -- round a word up to the nearest aligned value */

Word (WordAlignUp)(Word word, Align align)
{
  AVERT(Align, align);
  return WordAlignUp(word, align);
}

/* WordRoundUp -- round word up to round.
 *
 * .wordroundup.arg.word: The word arg is quantity to be rounded.
 * .wordroundup.arg.round: The modulus argument is not necessarily an
 * alignment (i.e., not a power of two).
 *
 * .wordroundup.result: Let m be congruent to 0 mod r (m == 0(r)), and
 * let m be the least m >= w.  If w+r-1 (!) is representable in Word
 * then result is m.  Otherwise result is 0.  Wittily.  (NB.  Result may
 * be 0 even if m is representable.)  */

Word (WordRoundUp)(Word word, Size modulus)
{
  AVER(modulus > 0);
  return WordRoundUp(word, modulus);
}


/* WordAlignUp -- round a word down to the nearest aligned value */

Word (WordAlignDown)(Word word, Align alignment)
{
  AVERT(Align, alignment);
  return WordAlignDown(word, alignment);
}


/* SizeIsP2 -- test whether a size is a power of two */

Bool (SizeIsP2)(Size size)
{
  return SizeIsP2(size);
}

/* WordIsP2 -- tests whether a word is a power of two */

Bool (WordIsP2)(Word word)
{
  return WordIsP2(word);
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


/* AddrAlignDown -- round a word down to the nearest aligned value */

Addr (AddrAlignDown)(Addr addr, Align alignment)
{
  AVERT(Align, alignment);
  return AddrAlignDown(addr, alignment);
}


/* ResIsAllocFailure
 *
 * Test whether a result code is in the set of allocation failure codes.  */

Bool ResIsAllocFailure(Res res)
{
  return (res == ResMEMORY || res == ResRESOURCE || res == ResCOMMIT_LIMIT);
}


/* WriteULongest -- output a textual representation of an integer to a stream
 *
 * Output as an unsigned value in the given base (2-16), padded to the
 * given width.  */

static Res WriteULongest(mps_lib_FILE *stream, ULongest w, unsigned base,
                         unsigned width)
{
  static const char digit[16 + 1] = "0123456789ABCDEF";
    /* + 1 for terminator: unused, but prevents compiler warning */
  static const char pad = '0'; /* padding character */
  char buf[MPS_WORD_WIDTH + 1]; /* enough for binary, */
                                /* plus one for terminator */
  unsigned i;
  int r;

  AVER(stream != NULL);
  AVER(2 <= base);
  AVER(base <= 16);
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
  if (r == mps_lib_EOF)
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
 * .write.double.check: There being no DBL_EXP_DIG, we assume that it is
 * less than DBL_DIG.  */

static Res WriteDouble(mps_lib_FILE *stream, double d)
{
  double F = d;
  int E = 0, i, x = 0;
  /* Largest exponent that will print in %f style.  Larger will use %e */
  /* style.  DBL_DIG is chosen for use of doubles as extra-large integers. */
  int expmax = DBL_DIG;
  /* Smallest exponent that will print in %f style.  Smaller will use */
  /* %e style.  -4 is chosen because it is the %g default. */
  int expmin = -4;
  /* Epsilon defines how many digits will be printed.  Using DBL_EPSILON */
  /* prints all the significant digits.  To print fewer digits, set */
  /* epsilon to 10 ^ - N, where N is the desired number of digits. */
  double epsilon = DBL_EPSILON / 2;
  char digits[] = "0123456789";
  /* sign, DBL_DIG, '0.', 'e', '+/-', log10(DBL_MAX_10_EXP), */
  /* terminator.  See .write.double.check. */
  char buf[1+DBL_DIG+2+1+1+DBL_DIG+1];
  int j = 0;
 
  if (F == 0.0) {
    if (mps_lib_fputs("0", stream) == mps_lib_EOF)
      return ResIO;
    return ResOK;
  }

  if (F < 0) {
    buf[j] = '-';
    j++;
    F = - F;
  }
 
  /* This scaling operation could introduce rounding errors. */
  for ( ; F >= 1.0 ; F /= 10.0) {
    E++;
    if (E > DBL_MAX_10_EXP) {
      if (mps_lib_fputs("Infinity", stream) == mps_lib_EOF)
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

  /* Convert the fraction to base 10, inserting a decimal according to */
  /* the exponent.  This is Steele and White's FP3 algorithm. */
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

  /* If %e notation is selected, append the exponent indicator and sign. */
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

    /* Format the exponent to at least two digits. */
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
 
  if (mps_lib_fputs(buf, stream) == mps_lib_EOF)
    return ResIO;
  return ResOK;
}


/* WriteF -- write formatted output
 *
 * .writef.des: See <design/writef/>, also <design/lib/>
 *
 * .writef.p: There is an assumption that void * fits in Word in
 * the case of $P, and ULongest for $U and $B.  This is checked in
 * MPMCheck.
 *
 * .writef.div: Although MPS_WORD_WIDTH/4 appears three times, there
 * are effectively three separate decisions to format at this width.
 *
 * .writef.check: See .check.writef.
 */

Res WriteF(mps_lib_FILE *stream, Count depth, ...)
{
  Res res;
  va_list args;
 
  va_start(args, depth);
  res = WriteF_v(stream, depth, args);
  va_end(args);
  return res;
}

Res WriteF_v(mps_lib_FILE *stream, Count depth, va_list args)
{
  const char *firstformat;
  Res res;

  firstformat = va_arg(args, const char *);
  res = WriteF_firstformat_v(stream, depth, firstformat, args);
  return res;
}

Res WriteF_firstformat_v(mps_lib_FILE *stream, Count depth, 
                         const char *firstformat, va_list args)
{
  const char *format;
  int r;
  size_t i;
  Res res;
  Bool start_of_line = TRUE;

  AVER(stream != NULL);

  format = firstformat;
 
  for(;;) {
    if (format == NULL)
      break;

    while(*format != '\0') {
      if (start_of_line) {
        for (i = 0; i < depth; ++i) {
          mps_lib_fputc(' ', stream);
        }
        start_of_line = FALSE;
      }
      if (*format != '$') {
        r = mps_lib_fputc(*format, stream); /* Could be more efficient */
        if (r == mps_lib_EOF)
          return ResIO;
        if (*format == '\n') {
          start_of_line = TRUE;
        }
      } else {
        ++format;
        AVER(*format != '\0');

        switch(*format) {
          case 'A': {                   /* address */
            WriteFA addr = va_arg(args, WriteFA);
            res = WriteULongest(stream, (ULongest)addr, 16,
                                (sizeof(WriteFA) * CHAR_BIT + 3) / 4);
            if (res != ResOK)
              return res;
          } break;

          case 'P': {                   /* pointer, see .writef.p */
            WriteFP p = va_arg(args, WriteFP);
            res = WriteULongest(stream, (ULongest)p, 16,
                                (sizeof(WriteFP) * CHAR_BIT + 3)/ 4);
            if (res != ResOK)
              return res;
          } break;

          case 'F': {                   /* function */
            WriteFF f = va_arg(args, WriteFF);
            Byte *b = (Byte *)&f;
            /* ISO C forbids casting function pointers to integer, so
               decode bytes (see design.writef.f). 
               TODO: Be smarter about endianness. */
            for(i=0; i < sizeof(WriteFF); i++) {
              res = WriteULongest(stream, (ULongest)(b[i]), 16,
                                  (CHAR_BIT + 3) / 4);
              if (res != ResOK)
                return res;
            }
          } break;
           
          case 'S': {                   /* string */
            WriteFS s = va_arg(args, WriteFS);
            r = mps_lib_fputs((const char *)s, stream);
            if (r == mps_lib_EOF)
              return ResIO;
          } break;
       
          case 'C': {                   /* character */
            WriteFC c = va_arg(args, WriteFC); /* promoted */
            r = mps_lib_fputc((int)c, stream);
            if (r == mps_lib_EOF)
              return ResIO;
          } break;
       
          case 'W': {                   /* word */
            WriteFW w = va_arg(args, WriteFW);
            res = WriteULongest(stream, (ULongest)w, 16,
                                (sizeof(WriteFW) * CHAR_BIT + 3) / 4);
            if (res != ResOK)
              return res;
          } break;

          case 'U': {                   /* decimal, see .writef.p */
            WriteFU u = va_arg(args, WriteFU);
            res = WriteULongest(stream, (ULongest)u, 10, 0);
            if (res != ResOK)
              return res;
          } break;

          case '3': {                   /* decimal for thousandths */
            WriteFU u = va_arg(args, WriteFU);
            res = WriteULongest(stream, (ULongest)u, 10, 3);
            if (res != ResOK)
              return res;
          } break;

          case 'B': {                   /* binary, see .writef.p */
            WriteFB b = va_arg(args, WriteFB);
            res = WriteULongest(stream, (ULongest)b, 2, sizeof(WriteFB) * CHAR_BIT);
            if (res != ResOK)
              return res;
          } break;
       
          case '$': {                   /* dollar char */
            r = mps_lib_fputc('$', stream);
            if (r == mps_lib_EOF)
              return ResIO;
          } break;

          case 'D': {                   /* double */
            WriteFD d = va_arg(args, WriteFD);
            res = WriteDouble(stream, d);
            if (res != ResOK)
              return res;
          } break;
              
          default:
          NOTREACHED;
        }
      }

      ++format;
    }

    format = va_arg(args, const char *);
  }
 
  return ResOK;
}


/* StringLength -- slow substitute for strlen */

size_t StringLength(const char *s)
{
  size_t i;

  AVER(s != NULL);

  for(i = 0; s[i] != '\0'; i++)
    NOOP;
  return(i);
}


/* StringEqual -- slow substitute for (strcmp == 0) */

Bool StringEqual(const char *s1, const char *s2)
{
  Index i;

  AVER(s1);
  AVER(s2);

  for(i = 0; ; i++) {
    if(s1[i] != s2[i])
      return FALSE;
    if(s1[i] == '\0') {
      AVER(s2[i] == '\0');
      break;
    }
  }
  return TRUE;
}



/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
