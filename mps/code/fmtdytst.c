/* fmtdytst.c: DYLAN FORMAT TEST CODE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .readership: MPS developers, Dylan developers.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "mps.h"
#include "testlib.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define unused(param)   ((void)param)

#ifdef MPS_BUILD_MV
/* windows.h causes warnings about "unreferenced inline function */
/* has been removed". */
#pragma warning(disable: 4514)
#endif /* MPS_BUILD_MV */

#ifdef MPS_PF_SUS8LC
/* .hack.malloc: builder.lc (LCC) uses Sun's header files.  Sun's
 * stdlib.h is broken, as it has an incorrect declaration of malloc.
 * We fix that here in a very hacky way.
 */
#define malloc(x) (void *)malloc(x)
#endif /* MPS_PF_SUS8LC */


static mps_word_t *ww = NULL;
static mps_word_t *tvw;


static mps_word_t dylan_make_WV(mps_word_t version, mps_word_t vb,
                                mps_word_t es, mps_word_t vf)
{
  assert((version & ((1 << 8) - 1)) == version);
  assert((vb & ((1 << 8) - 1)) == vb);
  assert((es & ((1 << 5) - 1)) == es);
  assert((vf & ((1 << 3) - 1)) == vf);

  /* VERSION- ... VB------ reserved ES---VF- */
  return((version << (MPS_WORD_WIDTH - 8)) |
	 (vb << 16) |
	 (es << 3) |
	 vf);
}


mps_res_t dylan_init(mps_addr_t addr, size_t size,
                     mps_addr_t *refs, size_t nr_refs)
{

  /* Make sure the size is aligned. */
  assert((size & (ALIGN-1)) == 0);

  if(ww == NULL) {
    ww = malloc(sizeof(mps_word_t) * (BASIC_WRAPPER_SIZE + 1));
    if(ww == NULL) return MPS_RES_MEMORY;
    tvw = malloc(sizeof(mps_word_t) * BASIC_WRAPPER_SIZE);
    if(tvw == NULL) {
      free(ww);
      return MPS_RES_MEMORY;
    }

    /* Build a wrapper wrapper. */
    ww[WW] = (mps_word_t)ww;
    ww[WC] = (mps_word_t)ww;     /* dummy class */
    ww[WM] = (1 << 2) | 1;       /* dummy subtype_mask */
    ww[WF] = ((WS - 1) << 2) | 2;
    ww[WV] = dylan_make_WV(2, 0, 0, 0);
    ww[WS] = (1 << 2) | 1;
    ww[WP] = 1;

    /* Build a wrapper for traceable vectors. */
    tvw[WW] = (mps_word_t)ww;
    tvw[WC] = (mps_word_t)ww;    /* dummy class */
    tvw[WM] = (1 << 2) | 1;      /* dummy subtype_mask */
    tvw[WF] = 0;                 /* no fixed part */
    tvw[WV] = dylan_make_WV(2, 0, 0, 2); /* traceable variable part */
    tvw[WS] = 1;                 /* no patterns */
  }

  /* If there is enough room, make a vector, otherwise just */
  /* make a padding object. */

  if(size >= sizeof(mps_word_t) * 2) {
    mps_word_t *p = (mps_word_t *)addr;
    mps_word_t i, t = (size / sizeof(mps_word_t)) - 2;

    p[0] = (mps_word_t)tvw;     /* install vector wrapper */
    p[1] = (t << 2) | 1;        /* tag the vector length */
    for(i = 0; i < t; ++i) {
      mps_word_t r = rnd();

      if(r & 1)
        p[2+i] = ((r & ~(mps_word_t)3) | 1); /* random int */
      else
        p[2+i] = (mps_word_t)refs[(r >> 1) % nr_refs]; /* random ptr */
    }
  } else
    dylan_pad(addr, size);

  return MPS_RES_OK;
}


void dylan_write(mps_addr_t addr, mps_addr_t *refs, size_t nr_refs)
{
  mps_word_t *p = (mps_word_t *)addr;
  mps_word_t t = p[1] >> 2;

  /* If the object is a vector, update a random entry. */
  if(p[0] == (mps_word_t)tvw && t > 0) {
    mps_word_t r = rnd();
    size_t i = 2 + (rnd() % t);

    if(r & 1)
      p[i] = ((r & ~(mps_word_t)3) | 1); /* random int */
    else
      p[i] = (mps_word_t)refs[(r >> 1) % nr_refs]; /* random ptr */
  }
}


mps_addr_t dylan_read(mps_addr_t addr)
{
  mps_word_t *p = (mps_word_t *)addr;

  /* If the object is a vector, return a random entry. */
  if(p[0] == (mps_word_t)tvw) {
    mps_word_t t = p[1] >> 2;
    if(t > 0)
      return (mps_addr_t)p[2 + (rnd() % t)];
  }

  return addr;
}

mps_bool_t dylan_check(mps_addr_t addr)
{
  assert(addr != 0);
  assert(((mps_word_t)addr & (ALIGN-1)) == 0);
  assert(dylan_wrapper_check((mps_word_t *)((mps_word_t *)addr)[0]));
  /* .assert.unused: Asserts throw away their conditions */
  /* in hot varieties, so UNUSED is needed. */
  unused(addr);
  return 1;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
