/* impl.c.fmthe: DYLAN-LIKE OBJECT FORMAT WITH HEADERS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .source: This was derived from impl.c.fmtdy -- it's probably a good idea to
 * keep them in sync and share improvements.
 *
 * .layouts:
 *
 *  All objects, B:
 *
 *  B           W               pointer to wrapper
 *  B+1         object body
 *
 *  Forwarded (or padding) one-word objects, B:
 *
 *  B           N | 0b01        new address | 1
 *
 *  Forwarded (or padding) multi-word objects, B:
 *
 *  B           N | 0b10        new address | 2
 *  B+1         L               limit of object (addr of end + 1)
 *
 *  Wrappers, W:
 *
 *  W           WW              pointer to wrapper wrapper
 *  W+1         class           DylanWorks class pointer (traceable)
 *  W+2         subtype_mask    DylanWorks subtype_mask (untraceable)
 *  W+3         (FL << 2) | FF  fixed part length and format
 *  W+4         (VS << 3) | VF  variable part format and element size
 *  W+5         (WT << 2) | 1   tagged pattern vector length
 *  W+6         pattern 0       patterns for fixed part fields
 *  W+6+WT-1    pattern WT-1
 *
 *  The wrapper wrapper, WW:
 *
 *  WW          WW              WW is it's own wrapper
 *  WW+1        class           DylanWorks class of wrappers
 *  WW+2        subtype_mask    DylanWorks subtype_mask for WW
 *  WW+3        (4 << 2) | 2    wrappers have four patterned fields
 *  WW+4        (0 << 3) | 0    wrappers have a non-traceable vector
 *  WW+5        (1 << 2) | 1    one pattern word follows
 *  WW+6        0b001           only field 0 is traceable
 *
 *
 * TRANSGRESSIONS
 *
 * .assert: Test code really shouldn't use assert.
 */


#include "fmthe.h"
#include "mps.h"

#include <assert.h>
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


#define notreached()    assert(0)
#define unused(param)   ((void)(param))


#ifdef FMTDY_COUNTING
#define FMTDY_COUNT(x) x
#define FMTDY_FL_LIMIT 16
static unsigned long dylan_vff_counts[4*8];
static unsigned long dylan_fl_counts[FMTDY_FL_LIMIT];
static unsigned long dylan_fl_oversize_count;
static unsigned long dylan_fw_counts[2];
#else
#define FMTDY_COUNT(x)
#endif /* FMTDY_COUNTING */


static int dylan_wrapper_check(mps_word_t *w)
{
  mps_word_t *ww;
  mps_word_t vh;
  mps_word_t version;
  mps_word_t reserved;
  mps_word_t class;
  mps_word_t fh, fl, ff;
  mps_word_t vb, es, vf;
  mps_word_t vt, t;

  assert(w != NULL);
  assert(((mps_word_t)w & 3) == 0);

  /* The first word of the wrapper is a pointer to a wrapper wrapper, */
  /* which always has the same contents.  Check it. */

  /* .improve.unique.wrapper: When this becomes part of the Dylan
   * run-time, it would be possible to know the address of a unique
   * wrapper wrapper and check that instead. */

  assert(w[WW] != 0);
  assert((w[WW] & 3) == 0);          /* wrapper wrapper is aligned */
  ww = (mps_word_t *)w[WW];
  assert(ww[WW] == w[WW]);           /* wrapper wrapper is own wrapper */
  assert(ww[WC] != 0);               /* wrapper class exists */
  assert((ww[WC] & 3) == 0);         /* wrapper class is aligned */
  assert(ww[WF] == (((WS - 1) << 2) | 2));  /* fields with patterns */
  assert((ww[WV] & 0x00ffffff) == 0);/* non-traceable vector */
  /* Code in this file only works for version 2 */
  assert(((ww[WV] >> (MPS_WORD_WIDTH - 8)) & 0xff) == 2);
  assert(ww[WS] == ((1 << 2) | 1));  /* one pattern word in wrapper wrapper */
  /* The first field is traceable, the second field can be traced, */
  /* but doesn't need to be. */
  assert((ww[WP] == 1) || (ww[WP] == 3));

  /* Unpack the wrapper. */

  class = w[WC];         /* class */
  fh = w[WF];            /* fixed part header word */
  fl = fh >> 2;         /* fixed part length */
  ff = fh & 3;          /* fixed part format code */
  vh = w[WV];            /* variable part header */
  version = (vh >> (MPS_WORD_WIDTH - 8)) & 0xff;
  assert(version == 2); /* Code in this file only works for version 2 */
  reserved = (vh >> 8) & 0xff;
  assert(reserved == 0);
  vb = (vh >> 16) & 0xff;
  es = (vh & 0xff) >> 3;/* element size */
  vf = vh & 7;          /* variable part format code */
  vt = w[WS];            /* vector total word (Dylan-tagged) */
  t = vt >> 2;          /* vector total length */

  /* The second word is the class of the wrapped object. */
  /* It would be good to check which pool this is in. */

  assert(class != 0);                   /* class exists */
  assert((class & 3) == 0);             /* class is aligned */

  /* The third word contains the fixed part format and length. */
  /* The only illegal format is 3.  Anything else is possible, although */
  /* we could do some bound checking on the length if we knew more about */
  /* the surroundings of the object. */

  /* Fixed part format 3 is reserved. */
  assert(ff != 3);

  /* Zero length fixed part is only legal in format 0. */
  /* Current Dylan run-time does not honour this so I remove it for now */
  /* We probably want this check as then we can scan without having to */
  /* check for 0 fixed length fields as a special case */
  /* assert(ff == 0 || fl != 0); */

  /* The fourth word contains the variable part format and element */
  /* size.  This assumes that DylanWorks is only going to use byte */
  /* vectors in the non-word case. */

  /* Variable part format 6 is reserved. */
  assert(vf != 6);

  /* There should be no shift in word vector formats. */
  assert((vf & 6) == 4 || es == 0);

  /* The fifth word is the number of patterns in the pattern */
  /* vector.  This can be calculated from the fixed part length. */
  /* The word is also tagged like a DylanWorks integer. */

  assert((vt & 3) == 1);

  /* The pattern vector in the wrapper should be of non-zero length */
  /* only if there is a patterned fixed part. */
  assert(ff == 2 || t == 0);

  /* The number of patterns is (fixed fields+31)/32. */
  assert(ff != 2 || t == ((fl + MPS_WORD_WIDTH - 1) >> MPS_WORD_SHIFT));

  /* The patterns are random bits, so we can't check them.  However, */
  /* the left-over bits in the last pattern should be zero. */

  assert(ff != 2 || (w[WS+t] >> ((fh>>2) & (MPS_WORD_WIDTH-1))) == 0);

  return 1;
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

/* Scan a contiguous array of references in [base, limit). */
/* This code has been hand-optimised and examined using Metrowerks */
/* Codewarrior on a 68K and also Microsoft Visual C on a 486.  The */
/* variables in the loop allocate nicely into registers.  Alter with */
/* care. */

static mps_res_t dylan_scan_contig(mps_ss_t mps_ss,
                                   mps_addr_t *base, mps_addr_t *limit)
{
  mps_res_t res;
  mps_addr_t *p;        /* reference cursor */
  mps_addr_t r;         /* reference to be fixed */

  MPS_SCAN_BEGIN(mps_ss) {
          p = base;
    loop: if(p >= limit) goto out;
          r = *p++;
          if(((mps_word_t)r&3) != 0) /* pointers tagged with 0 */
            goto loop;             /* not a pointer */
          if(!MPS_FIX1(mps_ss, r)) goto loop;
          res = MPS_FIX2(mps_ss, p-1);
          if(res == MPS_RES_OK) goto loop;
          return res;
    out:  assert(p == limit);
  } MPS_SCAN_END(mps_ss);

  return MPS_RES_OK;
}

/* dylan_weak_dependent -- returns the linked object, if any.
 */

extern mps_addr_t dylan_weak_dependent(mps_addr_t parent)
{
  mps_word_t *object;
  mps_word_t *wrapper;
  mps_word_t fword;
  mps_word_t fl;
  mps_word_t ff;

  assert(parent != NULL);
  object = (mps_word_t *)parent;
  wrapper = (mps_word_t *)object[0];
  assert(dylan_wrapper_check(wrapper));
  fword = wrapper[3];
  ff = fword & 3;
  /* traceable fixed part */
  assert(ff == 1);
  fl = fword & ~3uL;
  /* at least one fixed field */
  assert(fl >= 1);
  return (mps_addr_t) object[1];
}


/* Scan weakly a contiguous array of references in [base, limit). */
/* Only required to scan vectors for Dylan Weak Tables. */
/* Depends on the vector length field being scannable (ie a tagged */
/* integer). */
/* When a reference that has been fixed to NULL is detected the */
/* corresponding reference in the associated table (pointed to be the */
/* assoc variable) will be deleted. */

static mps_res_t
dylan_scan_contig_weak(mps_ss_t mps_ss,
		       mps_addr_t *base, mps_addr_t *limit,
		       mps_addr_t *objectBase, mps_addr_t *assoc)
{
  mps_addr_t *p;
  mps_res_t res;
  mps_addr_t r;

  MPS_SCAN_BEGIN(mps_ss) {
    p = base;
    goto skip_inc;
  loop:
    ++p;
  skip_inc:
    if(p >= limit)
      goto out;
    r = *p;
    if(((mps_word_t)r & 3) != 0) /* non-pointer */
      goto loop;
    if(!MPS_FIX1(mps_ss, r))
      goto loop;
    res = MPS_FIX2(mps_ss, p);
    if(res == MPS_RES_OK) {
      if(*p == 0 && r != 0) {
	if(assoc != NULL) {
	  assoc[p-objectBase] = 0;	/* delete corresponding entry */
	}
      }
      goto loop;
    }
    return res;
  out:
    assert(p == limit);
  } MPS_SCAN_END(mps_ss);

  return MPS_RES_OK;
}


/* dylan_scan_pat -- scan according to pattern
 *
 * Scan an array of words in [base, limit) using the patterns at pats
 * to determine which words can be fixed.
 */

static mps_res_t dylan_scan_pat(mps_ss_t mps_ss,
                                mps_addr_t *base, mps_addr_t *limit,
                                mps_word_t *pats, mps_word_t nr_pats)
{
  mps_res_t res;
  mps_word_t *pc = pats;/* pattern cursor */
  mps_word_t pat;       /* pattern register */
  mps_addr_t *p;        /* reference cursor */
  mps_addr_t *pp;       /* inner loop cursor */
  int b;                /* bit */
  mps_addr_t r;         /* reference to be fixed */

  unused(nr_pats);

  MPS_SCAN_BEGIN(mps_ss) {
          p = base;
          goto in;
    pat:  p += MPS_WORD_WIDTH;
          if(p >= limit) goto out;
    in:   pp = p;
          pat = *pc++;
    loop: if(pat == 0) goto pat;
          ++pp;
          b = (int)(pat & 1);
          pat >>= 1;
          if(b == 0) goto loop;
          r = *(pp-1);
          if(((mps_word_t)r&3) != 0) /* pointers tagged with 0 */
            goto loop;             /* not a pointer */
          if(!MPS_FIX1(mps_ss, r)) goto loop;
          res = MPS_FIX2(mps_ss, pp-1);
          if(res == MPS_RES_OK) goto loop;
          return res;
    out:  assert(p < limit + MPS_WORD_WIDTH);
          assert(pc == pats + nr_pats);
  } MPS_SCAN_END(mps_ss);

  return MPS_RES_OK;
}


#define AddHeader(p) ((mps_addr_t)((char*)(p) + headerSIZE))


#define NONWORD_LENGTH(_vt, _es) \
  ((_es) < MPS_WORD_SHIFT ? \
   ((_vt) + (1 << (MPS_WORD_SHIFT - (_es))) - 1) >> \
     (MPS_WORD_SHIFT - (_es)) : \
   (_vt) << ((_es) - MPS_WORD_SHIFT))

static mps_res_t dylan_scan1(mps_ss_t mps_ss, mps_addr_t *object_io)
{
  mps_addr_t *p;        /* cursor in object */
  mps_addr_t *q;        /* cursor limit for loops */
  mps_word_t h;         /* header word */
  mps_word_t *w;        /* pointer to wrapper */
  mps_word_t fh;        /* fixed part header word */
  mps_word_t fl;        /* fixed part length, in words */
  mps_word_t vh;        /* variable part header */
  mps_word_t vf;        /* variable part format */
  mps_word_t vl;        /* variable part actual length */
  unsigned vb;          /* vector bias */
  unsigned es;          /* variable part element size (log2 of bits) */
  mps_word_t vt;        /* total vector length */
  mps_res_t res;
  int header;

  assert(object_io != NULL);

  p = (mps_addr_t *)*object_io;
  assert(p != NULL);

  header = *(int*)((char*)p - headerSIZE);
  switch(headerType(header)) {
  case realTYPE:
      break;
  case padTYPE:
      *object_io = (mps_addr_t)((char*)p + headerPadSize(header));
      return MPS_RES_OK;
  default:
      notreached();
      break;
  }

  h = (mps_word_t)p[0];         /* load the header word */

  /* If the object is forwarded, simply skip it. */
  if(h & 3) {
    mps_addr_t l;

    if((h & 3) == 1) {
      /* single-word */
      l = AddHeader(p + 1);
      FMTDY_COUNT(++dylan_fw_counts[0]);
    } else {                      /* multi-word */
      assert((h & 3) == 2);
      l = (mps_addr_t)p[1];
      FMTDY_COUNT(++dylan_fw_counts[1]);
    }

    *object_io = l;
    return MPS_RES_OK;
  }

  mps_fix(mps_ss, p);           /* fix the wrapper */
  w = (mps_word_t *)p[0];       /* wrapper is header word */
  assert(dylan_wrapper_check(w));

  ++p;                          /* skip header */

  /* Fixed Part */

  fh = w[WF];
  fl = fh >> 2;                 /* get the fixed part length */

  /* It might be worth inlining common cases here, for example, */
  /* pairs.  This can be done by examining fh as a whole. */

  FMTDY_COUNT(fl < FMTDY_FL_LIMIT ? ++dylan_fl_counts[fl] :
                                       ++dylan_fl_oversize_count);
  if(fl > 0) {
    q = p + fl;                 /* set q to end of fixed part */
    switch(fh & 3) {            /* switch on the fixed format */
      case 0:                   /* all non-traceable fields */
      p = q;
      break;

      case 1:                   /* all traceable fields */
      res = dylan_scan_contig(mps_ss, p, q);
      if(res) return res;
      break;

      case 2:                   /* patterns */
      res = dylan_scan_pat(mps_ss, p, q, &w[WP], w[WS]>>2);
      if(res) return res;
      break;

      default:
      notreached();
      break;
    }
    p = q;
  }

  /* Variable Part */
  vh = w[WV];
  vf = vh & 7;                  /* get variable part format */
  FMTDY_COUNT(++dylan_vff_counts[(vf << 2)|(fh&3)]);
  if(vf != 7)
  {
    vt = *(mps_word_t *)p;      /* total vector length */
    assert((vt & 3) == 1);      /* check Dylan integer tag */
    vt >>= 2;                   /* untag it */
    ++p;

    switch(vf)
    {
      case 0:                   /* non-stretchy non-traceable */
      p += vt;
      break;

      case 1:                   /* stretchy non-traceable */
      notreached();             /* Not used by DylanWorks yet */
      p += vt + 1;
      break;

      case 2:                   /* non-stretchy traceable */
      q = p + vt;
      res = dylan_scan_contig(mps_ss, p, q);
      if(res) return res;
      p = q;
      break;

      case 3:                   /* stretchy traceable */
      notreached();             /* DW doesn't create them yet */
      vl = *(mps_word_t *)p;    /* vector length */
      assert((vl & 3) == 1);    /* check Dylan integer tag */
      vl >>= 2;                 /* untag it */
      ++p;
      res = dylan_scan_contig(mps_ss, p, p + vl);
      if(res) return res;
      p += vt;                  /* skip to end of whole vector */
      break;

      case 4:                   /* non-word */
      es = (vh & 0xff) >> 3;
      vb = (vh >> 16) & 0xff;
      vt += vb;
      p += NONWORD_LENGTH(vt, es);
      break;

      case 5:                   /* stretchy non-word */
      notreached();             /* DW doesn't create them yet */
      es = (vh & 0xff) >> 3;
      vb = (vh >> 16) & 0xff;
      vt += vb;
      p += NONWORD_LENGTH(vt, es) + 1;
      break;

      default:
      notreached();
      break;
    }
  }

  *object_io = AddHeader(p);
  return MPS_RES_OK;
}


static mps_res_t dylan_scan(mps_ss_t mps_ss,
                            mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;
  mps_addr_t p = base;

  while(p < limit) {
    res = dylan_scan1(mps_ss, &p);
    if(res) return res;
  }

  assert(p <= AddHeader(limit));

  return MPS_RES_OK;
}


static mps_res_t dylan_scan1_weak(mps_ss_t mps_ss, mps_addr_t *object_io)
{
  mps_addr_t *assoc;
  mps_addr_t *base;
  mps_addr_t *p, q;
  mps_res_t res;
  mps_word_t *w;
  mps_word_t fword, ff, fl;
  mps_word_t h;
  mps_word_t vword, vf, vl;
  int header;

  assert(object_io != NULL);
  base = (mps_addr_t *)*object_io;
  assert(base != NULL);
  p = base;

  header = *(int*)((char*)p - headerSIZE);
  switch(headerType(header)) {
  case realTYPE:
      break;
  case padTYPE:
      *object_io = (mps_addr_t)((char*)p + headerPadSize(header));
      return MPS_RES_OK;
  default:
      notreached();
      break;
  }

  h = (mps_word_t)p[0];
  /* object should not be forwarded (as there is no forwarding method) */
  assert((h & 3) == 0);

  mps_fix(mps_ss, p);

  /* w points to wrapper */
  w = (mps_word_t *)p[0];

  assert(dylan_wrapper_check(w));

  ++p;			/* skip header */

  fword = w[WF];
  fl = fword >> 2;
  /* weak vectors should have at least one fixed field */
  /* (for assoc field) */
  assert(fl >= 1);

  ff = fword & 3;

  /* weak vectors should have traceable fixed format */
  assert(ff == 1);

  assoc = (mps_addr_t *)p[0];

  vword = w[WV];
  vf = vword & 7;
  vl = (mps_word_t)p[fl] >> 2;

  /* weak vectors should be non-stretchy traceable */
  assert(vf == 2);

  /* q is end of the object.  There are fl fixed fields, vl variable */
  /* fields and another slot that contains the vector length */
  q = p + fl + vl + 1;

  res = dylan_scan_contig_weak(mps_ss, p, q, base, assoc);
  if(res != MPS_RES_OK) {
    return res;
  }

  *object_io = AddHeader(q);
  return MPS_RES_OK;
}


static mps_res_t dylan_scan_weak(mps_ss_t mps_ss,
				 mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;

  while(base < limit) {
    res = dylan_scan1_weak(mps_ss, &base);
    if(res) return res;
  }

  assert(base <= AddHeader(limit));

  return MPS_RES_OK;
}

static mps_addr_t dylan_skip(mps_addr_t object)
{
  mps_addr_t *p;        /* cursor in object */
  mps_word_t *w;        /* wrapper cursor */
  mps_word_t h;         /* header word */
  mps_word_t vh;        /* variable part header */
  mps_word_t vf;        /* variable part format */
  mps_word_t vt;        /* total vector length */
  unsigned vb;          /* vector bias */
  unsigned es;          /* variable part element size (log2 of bits) */
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

  h = (mps_word_t)p[0];         /* load the header word */

  /* If the object is forwarded, simply skip it. */
  if(h & 3) {
    if((h & 3) == 1)            /* single-word */
      return AddHeader(p + 1);
    else {                      /* multi-word */
      assert((h & 3) == 2);
      return (mps_addr_t)p[1];
    }
  }

  w = (mps_word_t *)h;          /* load the fixed wrapper */
  assert(dylan_wrapper_check(w));
  ++p;

  p += w[WF] >> 2;               /* skip fixed part fields */

  vh = w[WV];
  vf = vh & 7;                  /* get variable part format */
  if(vf != 7)
  {
    vt = *(mps_word_t *)p;
    assert((vt & 3) == 1);      /* check Dylan integer tag */
    vt = vt >> 2;               /* total length */
    ++p;

    p += vf & 1;                /* stretchy vectors have an extra word */

    if((vf & 6) == 4)           /* non-word */
    {
      es = (vh & 0xff) >> 3;
      vb = (vh >> 16) & 0xff;
      vt += vb;
      p += NONWORD_LENGTH(vt, es);
    }
    else
      p += vt;
  }

  return AddHeader(p);
}


static mps_addr_t dylan_isfwd(mps_addr_t object)
{
  mps_word_t h, tag;
  int header;

  header = *(int*)((char*)object - headerSIZE);
  if (headerType(header) != realTYPE)
    return NULL;

  h = *(mps_word_t *)object;
  tag = h & 3;
  if(tag != 0)
    return (mps_addr_t)(h - tag);
  else
    return NULL;
}


static void dylan_fwd(mps_addr_t old, mps_addr_t new)
{
  mps_word_t *p;
  mps_addr_t limit;

  assert(dylan_isfwd(old) == NULL);
  assert(((mps_word_t)new & 3) == 0);

  p = (mps_word_t *)old;
  limit = dylan_skip(old);
  if(limit == &p[1])    /* single-word object? */
    p[0] = (mps_word_t)new | 1;
  else {
    p[0] = (mps_word_t)new | 2;
    p[1] = (mps_word_t)limit;
  }
}


static void dylan_pad(mps_addr_t addr, size_t fullSize)
{
  *(int*)addr = padHeader(fullSize);
}


static mps_addr_t dylan_no_isfwd(mps_addr_t object)
{
  unused(object);
  notreached();
  return 0;
}

static void dylan_no_fwd(mps_addr_t old, mps_addr_t new)
{
  unused(old); unused(new);
  notreached();
}

static void dylan_no_pad(mps_addr_t addr, size_t size)
{
  unused(addr); unused(size);
  notreached();
}

/* HeaderFormat -- format descriptor for this format */

static struct mps_fmt_auto_header_s HeaderFormat =
{
  ALIGN,
  dylan_scan,
  dylan_skip,
  dylan_fwd,
  dylan_isfwd,
  dylan_pad,
  (size_t)headerSIZE
};


/* HeaderWeakFormat -- format descriptor for this format */

static struct mps_fmt_auto_header_s HeaderWeakFormat =
{
  ALIGN,
  dylan_scan_weak,
  dylan_skip,
  dylan_no_fwd,
  dylan_no_isfwd,
  dylan_no_pad,
  (size_t)headerSIZE
};


/* EnsureHeaderFormat -- create a format object for this format */

mps_res_t EnsureHeaderFormat(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
  return mps_fmt_create_auto_header(mps_fmt_o, arena, &HeaderFormat);
}


/* EnsureHeaderWeakFormat -- create a format object for the weak format */

mps_res_t EnsureHeaderWeakFormat(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
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
