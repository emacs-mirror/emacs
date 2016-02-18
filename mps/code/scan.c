/* scan.c: SCANNING FUNCTIONS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.
 * See end of file for license.
 *
 * .outside: The code in this file is written as if *outside* the MPS,
 * and so is restricted to facilities in the MPS interface.  MPS users
 * are invited to read this code and use it as a basis for their own
 * scanners.  See topic "Area Scanners" in the MPS manual.
 *
 * TODO: Design document.
 */

#include "mps.h"


/* mps_scan_area -- scan contiguous area of references
 *
 * This is a convenience function for scanning the contiguous area
 * [base, limit).  I.e., it calls Fix on all words from base up to
 * limit, inclusive of base and exclusive of limit.
 *
 * This scanner is appropriate for use when all words in the area are
 * simple untagged references.
 */

mps_res_t mps_scan_area(mps_ss_t ss,
                        mps_word_t *base, mps_word_t *limit,
                        void *closure, size_t closure_size)
{
  (void)closure; /* unused */
  (void)closure_size; /* unused */

  MPS_SCAN_BEGIN(ss) {
    mps_word_t *_p = base;
    while (_p < limit) {
      mps_word_t word = *_p;
      mps_addr_t ref = (mps_addr_t)word;
      if (MPS_FIX1(ss, ref)) {
        mps_res_t res = MPS_FIX2(ss, &ref);
        if (res != MPS_RES_OK)
          return res;
        *_p = (mps_word_t)ref;
      }
      ++_p;
    }
  } MPS_SCAN_END(ss);

  return MPS_RES_OK;
}


#define MPS_SCAN_AREA_TAGGED(test) \
  MPS_SCAN_BEGIN(ss) {                                  \
    mps_word_t *p = base;                               \
    while (p < limit) {                                 \
      mps_word_t word = *p;                             \
      mps_word_t tag_bits = word & mask;                \
      if (test) {                                       \
        mps_addr_t ref = (mps_addr_t)(word ^ tag_bits); \
        if (MPS_FIX1(ss, ref)) {                        \
          mps_res_t res = MPS_FIX2(ss, &ref);           \
          if (res != MPS_RES_OK)                        \
            return res;                                 \
          *p = (mps_word_t)ref | tag_bits;              \
        }                                               \
      }                                                 \
      ++p;                                              \
    }                                                   \
  } MPS_SCAN_END(ss);



/* mps_scan_area_masked -- scan area masking off tag bits
 *
 * Like mps_scan_area, but removes tag bits before fixing references,
 * and restores them afterwards.
 *
 * For example, if mask is 7, then this scanner will clear the bottom
 * three bits of each word before fixing.
 *
 * This scanner is useful when all words in the area must be treated
 * as references no matter what tag they have.
 */

mps_res_t mps_scan_area_masked(mps_ss_t ss,
                               mps_word_t *base, mps_word_t *limit,
                               void *closure, size_t closure_size)
{
  mps_scan_tag_t tag = closure;
  mps_word_t mask = tag->mask;
  (void)closure_size; /* unused */

  MPS_SCAN_AREA_TAGGED(1);

  return MPS_RES_OK;
}


/* mps_scan_area_tagged -- scan area selecting by tag
 *
 * Like mps_scan_area_masked, except only references whose masked bits
 * match a particular tag pattern are fixed.
 *
 * For example, if mask is 7 and pattern is 5, then this scanner will
 * only fix words whose low order bits are 0b101.
 */

mps_res_t mps_scan_area_tagged(mps_ss_t ss,
                               mps_word_t *base, mps_word_t *limit,
                               void *closure, size_t closure_size)
{
  mps_scan_tag_t tag = closure;
  mps_word_t mask = tag->mask;
  mps_word_t pattern = tag->pattern;
  (void)closure_size; /* unused */

  MPS_SCAN_AREA_TAGGED(tag_bits == pattern);

  return MPS_RES_OK;
}


/* mps_scan_area_tagged_or_zero -- scan area selecting by tag or zero
 *
 * Like mps_scan_area_tagged, except references whose masked bits are
 * zero are fixed in addition to those that match the pattern.
 *
 * For example, if mask is 7 and pattern is 3, then this scanner will
 * fix words whose low order bits are 0b011 and words whose low order
 * bits are 0b000, but not any others.
 *
 * This scanner is most useful for ambiguously scanning the stack and
 * registers when using an optimising C compiler and non-zero tags on
 * references, since the compiler is likely to leave untagged
 * addresses of objects around which must not be ignored.
 */

mps_res_t mps_scan_area_tagged_or_zero(mps_ss_t ss,
                                       mps_word_t *base, mps_word_t *limit,
                                       void *closure, size_t closure_size)
{
  mps_scan_tag_t tag = closure;
  mps_word_t mask = tag->mask;
  mps_word_t pattern = tag->pattern;
  (void)closure_size; /* unused */

  MPS_SCAN_AREA_TAGGED(tag_bits == 0 || tag_bits == pattern);

  return MPS_RES_OK;
}  


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited
 * <http://www.ravenbrook.com/>.
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
