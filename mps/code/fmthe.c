/* fmthe.c: DYLAN-LIKE OBJECT FORMAT WITH HEADERS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * Uses <code/fmtdy.c> for the actual Dylan format, and just adds
 * a thin layer to handle the object headers themselves.
 */

#include "fmtdy.h"
#include "fmtno.h"
#include "fmthe.h"
#include "mps.h"

#include <string.h>
#include <stdlib.h>

#include "mpstd.h"
#ifdef MPS_PF_SUS8LC
/* .hack.stderr: builder.lc (LCC) uses Sun's header files.  Sun's
 * assert.h is broken, as it assumes it can use stderr.  We have to
 * fix it by supplying stderr.
 */
#include <stdio.h>
/* Better include ossu.h as well, in case we use other stuff from stdio.h. */
#include "ossu.h"
#endif

#include "testlib.h"


#define assert(cond) Insist(cond)
#define notreached()    assert(0)

#define AddHeader(p) ((mps_addr_t)((char*)(p) + headerSIZE))

static mps_fmt_A_s *dylan_format;

static mps_res_t dylan_header_scan(mps_ss_t mps_ss,
                                   mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;
  mps_addr_t p = base;

  while(p < limit) {
      int header = *(int*)((char*)p - headerSIZE);
      switch(headerType(header)) {
      case realTYPE:
          break;
      case padTYPE:
          p = (mps_addr_t)((char*)p + headerPadSize(header));
          continue;
      default:
          notreached();
          break;
      }
      res = dylan_scan1(mps_ss, &p);
      if(res) return res;
      p = AddHeader(p);
  }

  assert(p <= AddHeader(limit));

  return MPS_RES_OK;
}


static mps_res_t dylan_header_scan_weak(mps_ss_t mps_ss,
                                        mps_addr_t base,
                                        mps_addr_t limit)
{
  mps_res_t res;

  while(base < limit) {
      int header;
      header = *(int*)((char*)base - headerSIZE);
      switch(headerType(header)) {
      case realTYPE:
          break;
      case padTYPE:
          base = (mps_addr_t)((char*)base + headerPadSize(header));
          continue;
      default:
          notreached();
          break;
      }

      res = dylan_scan1_weak(mps_ss, &base);
      if(res) return res;
      base = AddHeader(base);
  }

  assert(base <= AddHeader(limit));

  return MPS_RES_OK;
}

static mps_addr_t dylan_header_skip(mps_addr_t object)
{
  mps_addr_t *p;        /* cursor in object */
  int header;

  p = (mps_addr_t *)object;
  assert(p != NULL);

  header = *(int*)((char*)object - headerSIZE);
  switch(headerType(header)) {
  case realTYPE:
      break;
  case padTYPE:
      return (mps_addr_t)((char*)object + headerPadSize(header));
  default:
      notreached();
      break;
  }

  p = dylan_format->skip(object);
  p = AddHeader(p);
  return p;
}


static mps_addr_t dylan_header_isfwd(mps_addr_t object)
{
  int header;

  header = *(int*)((char*)object - headerSIZE);
  if (headerType(header) != realTYPE)
    return NULL;

  return dylan_format->isfwd(object);
}


static void dylan_header_pad(mps_addr_t addr, size_t fullSize)
{
  *(int*)addr = padHeader(fullSize);
}


/* HeaderFormat -- format descriptor for this format */

static struct mps_fmt_auto_header_s HeaderFormat =
{
  ALIGN,
  dylan_header_scan,
  dylan_header_skip,
  NULL, /* later overwritten by dylan format forward method */
  dylan_header_isfwd,
  dylan_header_pad,
  (size_t)headerSIZE
};


/* HeaderWeakFormat -- format descriptor for this format */

static struct mps_fmt_auto_header_s HeaderWeakFormat =
{
  ALIGN,
  dylan_header_scan_weak,
  dylan_header_skip,
  no_fwd,
  no_isfwd,
  no_pad,
  (size_t)headerSIZE
};


/* EnsureHeaderFormat -- create a format object for this format */

mps_res_t EnsureHeaderFormat(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
    dylan_format = dylan_fmt_A();
    HeaderFormat.fwd = dylan_format->fwd;
    return mps_fmt_create_auto_header(mps_fmt_o, arena, &HeaderFormat);
}


/* EnsureHeaderWeakFormat -- create a format object for the weak format */

mps_res_t EnsureHeaderWeakFormat(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
    dylan_format = dylan_fmt_A();
    return mps_fmt_create_auto_header(mps_fmt_o, arena, &HeaderWeakFormat);
}


/* HeaderFormatCheck -- check an object in this format */

mps_res_t HeaderFormatCheck(mps_addr_t addr)
{
    if (addr != 0 && ((mps_word_t)addr & (ALIGN-1)) == 0
        && dylan_wrapper_check((mps_word_t *)((mps_word_t *)addr)[0]))
        return MPS_RES_OK;
    else
        return MPS_RES_FAIL;
}

/* HeaderWeakFormatCheck -- check an object in this format */

mps_res_t HeaderWeakFormatCheck(mps_addr_t addr)
{
    if (addr != 0 && ((mps_word_t)addr & (ALIGN-1)) == 0
        && dylan_wrapper_check((mps_word_t *)((mps_word_t *)addr)[0]))
        return MPS_RES_OK;
    else
        return MPS_RES_FAIL;
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
