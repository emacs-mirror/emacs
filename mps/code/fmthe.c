/* impl.c.fmthe: DYLAN-LIKE OBJECT FORMAT WITH HEADERS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 * Copyright (c) 2002 Global Graphics Software.
 *
 * Uses impl.c.fmtdy for the actual Dylan format, and just adds
 * a thin layer to handle the object headers themselves.
 */

#include "fmtdy.h"
#include "fmtno.h"
#include "fmthe.h"
#include "mps.h"

#include <string.h>
#include <stdlib.h>

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
