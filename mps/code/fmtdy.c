/* fmtdy.c: DYLAN OBJECT FORMAT IMPLEMENTATION
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *  Portions copyright (c) 2002 Global Graphics Software.
 *
 * .readership: MPS developers, Dylan developers
 *
 *  .layouts:
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
 *  .improve.abstract.access: There are severe common subexpression
 *  problems.  In particular, code for accessing subfields in the
 *  fh and vh words is repeated.  It should be abstracted into
 *  macros (or functions).  This is particularly bad for the vh
 *  word which has 4 subfields (version, vb, es, vf).
 */


#include "fmtdy.h"
#include "fmtno.h"
#include "mps.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>


#define notreached()    assert(0)
#define unused(param)   ((void)param)


#ifdef _MSC_VER

/* MPS_END causes "constant conditional" warnings. */
#pragma warning(disable: 4127)

/* windows.h causes warnings about "unreferenced inline function */
/* has been removed". */
#pragma warning(disable: 4514)

#endif /* _MSC_VER */


#define ALIGN           sizeof(mps_word_t)

#define FMTDY_WORD_WIDTH (sizeof(mps_word_t) * CHAR_BIT)
#define FMTDY_WORD_SHIFT (FMTDY_WORD_WIDTH == 64 ? 6 : 5)
/* FMTDY_WORD_SHIFT is a bit hacky, but good enough for tests. */

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


int dylan_wrapper_check(mps_word_t *w)
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
  assert(((ww[WV] >> (FMTDY_WORD_WIDTH - 8)) & 0xff) == 2);
  assert(ww[WS] == ((1 << 2) | 1));  /* one pattern word in wrapper wrapper */
  /* The first field is traceable, the second field can be traced, */
  /* but doesn't need to be. */
  assert((ww[WP] == 1) || (ww[WP] == 3));
  unused(ww);
  
  /* Unpack the wrapper. */

  class = w[WC];         /* class */
  unused(class);
  fh = w[WF];            /* fixed part header word */
  fl = fh >> 2;         /* fixed part length */
  ff = fh & 3;          /* fixed part format code */
  vh = w[WV];            /* variable part header */
  version = (vh >> (FMTDY_WORD_WIDTH - 8)) & 0xff;
  assert(version == 2); /* Code in this file only works for version 2 */
  unused(version);
  reserved = (vh >> 8) & 0xff;
  assert(reserved == 0);
  unused(reserved);
  vb = (vh >> 16) & 0xff;
  unused(vb);
  es = (vh & 0xff) >> 3;/* element size */
  vf = vh & 7;          /* variable part format code */
  vt = w[WS];            /* vector total word (Dylan-tagged) */
  t = vt >> 2;          /* vector total length */
  unused(t);
  
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
  unused(ff);

  /* Zero length fixed part is only legal in format 0. */
  /* Current Dylan run-time does not honour this so I remove it for now */
  /* We probably want this check as then we can scan without having to */
  /* check for 0 fixed length fields as a special case */
  /* assert(ff == 0 || fl != 0); */
  unused(fl);
  /* The fourth word contains the variable part format and element */
  /* size.  This assumes that DylanWorks is only going to use byte */
  /* vectors in the non-word case. */

  /* Variable part format 6 is reserved. */
  assert(vf != 6);
  unused(vf);
  
  /* There should be no shift in word vector formats. */
  assert((vf & 6) == 4 || es == 0);
  unused(es);
  
  /* The fifth word is the number of patterns in the pattern */
  /* vector.  This can be calculated from the fixed part length. */
  /* The word is also tagged like a DylanWorks integer. */

  assert((vt & 3) == 1);

  /* The pattern vector in the wrapper should be of non-zero length */
  /* only if there is a patterned fixed part. */
  assert(ff == 2 || t == 0);

  /* The number of patterns is (fixed fields+31)/32. */
  assert(ff != 2 || t == ((fl + FMTDY_WORD_WIDTH - 1) / FMTDY_WORD_WIDTH));

  /* The patterns are random bits, so we can't check them.  However, */
  /* the left-over bits in the last pattern should be zero. */

  assert(ff != 2 || (w[WS+t] >> ((fh>>2) & (FMTDY_WORD_WIDTH-1))) == 0);

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
  unused(ff);
  fl = fword & ~(mps_word_t)3;
  /* at least one fixed field */
  assert(fl >= 1);
  unused(fl);
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
          assoc[p-objectBase] = 0;      /* delete corresponding entry */
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


/* dylan_scan_pat -- scan according to pattern */

/* Scan an array of words in [base, limit) using the patterns at pats */
/* to determine which words can be fixed. */
/* This code has been hand-optimised and examined using Metrowerks */
/* Codewarrior on a 68K and also Microsoft Visual C on a 486.  The */
/* variables in the loop allocate nicely into registers.  Alter with */
/* care. */

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
    pat:  p += FMTDY_WORD_WIDTH;
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
    out:  assert(p < limit + FMTDY_WORD_WIDTH);
          assert(pc == pats + nr_pats);
  } MPS_SCAN_END(mps_ss);

  return MPS_RES_OK;
}


#define NONWORD_LENGTH(_vt, _es) \
  ((_es) < FMTDY_WORD_SHIFT ? \
   ((_vt) + ((mps_word_t)1 << (FMTDY_WORD_SHIFT - (_es))) - 1) >> \
     (FMTDY_WORD_SHIFT - (_es)) : \
   (_vt) << ((_es) - FMTDY_WORD_SHIFT))


extern mps_res_t dylan_scan1(mps_ss_t mps_ss, mps_addr_t *object_io)
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

  assert(object_io != NULL);

  p = (mps_addr_t *)*object_io;
  assert(p != NULL);

  h = (mps_word_t)p[0];         /* load the header word */

  /* If the object is forwarded, simply skip it. */
  if(h & 3) {
    mps_addr_t l;

    if((h & 3) == 1) {
      /* single-word */
      l = (mps_addr_t)(p + 1);
      FMTDY_COUNT(++dylan_fw_counts[0]);
    } else {                      /* multi-word */
      assert((h & 3) == 2);
      l = (mps_addr_t)p[1];
      FMTDY_COUNT(++dylan_fw_counts[1]);
    }

    *object_io = l;
    return MPS_RES_OK;
  }

  res = mps_fix(mps_ss, p);     /* fix the wrapper */
  if ( res != MPS_RES_OK ) return res;
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
      p += vt + 1;
      notreached();             /* Not used by DylanWorks yet */
      break;

      case 2:                   /* non-stretchy traceable */
      q = p + vt;
      res = dylan_scan_contig(mps_ss, p, q);
      if(res) return res;
      p = q;
      break;

      case 3:                   /* stretchy traceable */
      vl = *(mps_word_t *)p;    /* vector length */
      assert((vl & 3) == 1);    /* check Dylan integer tag */
      vl >>= 2;                 /* untag it */
      ++p;
      res = dylan_scan_contig(mps_ss, p, p + vl);
      if(res) return res;
      p += vt;                  /* skip to end of whole vector */
      notreached();             /* DW doesn't create them yet */
      break;

      case 4:                   /* non-word */
      es = (vh & 0xff) >> 3;
      vb = (vh >> 16) & 0xff;
      vt += vb;
      p += NONWORD_LENGTH(vt, es);
      break;

      case 5:                   /* stretchy non-word */
      es = (vh & 0xff) >> 3;
      vb = (vh >> 16) & 0xff;
      vt += vb;
      p += NONWORD_LENGTH(vt, es) + 1;
      notreached();             /* DW doesn't create them yet */
      break;

      default:
      notreached();
      break;
    }
  }

  *object_io = (mps_addr_t)p;
  return MPS_RES_OK;
}

static mps_res_t dylan_scan(mps_ss_t mps_ss,
                            mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;

  while(base < limit) {
    res = dylan_scan1(mps_ss, &base);
    if(res) return res;
  }

  assert(base == limit);

  return MPS_RES_OK;
}

/* dylan_class -- return pointer indicating class of object
 *
 * Return wrapper pointer, except for broken hearts or padding
 */

static mps_addr_t dylan_class(mps_addr_t obj)
{
  mps_word_t first_word = ((mps_word_t *)obj)[0];

  if((first_word & 3) != 0) /* broken heart or padding */
    return NULL;
  else
    return (mps_addr_t)first_word;
}

extern mps_res_t dylan_scan1_weak(mps_ss_t mps_ss, mps_addr_t *object_io)
{
  mps_addr_t *assoc;
  mps_addr_t *base;
  mps_addr_t *p, q;
  mps_res_t res;
  mps_word_t *w;
  mps_word_t fword, ff, fl;
  mps_word_t h;
  mps_word_t vword, vf, vl;

  assert(object_io != NULL);
  base = (mps_addr_t *)*object_io;
  assert(base != NULL);
  p = base;

  h = (mps_word_t)p[0];
  /* object should not be forwarded (as there is no forwarding method) */
  assert((h & 3) == 0);
  unused(h);
  
  res = mps_fix(mps_ss, p);
  if ( res != MPS_RES_OK ) return res;

  /* w points to wrapper */
  w = (mps_word_t *)p[0];

  assert(dylan_wrapper_check(w));

  ++p;                  /* skip header */

  fword = w[WF];
  fl = fword >> 2;
  /* weak vectors should have at least one fixed field */
  /* (for assoc field) */
  assert(fl >= 1);

  ff = fword & 3;

  /* weak vectors should have traceable fixed format */
  assert(ff == 1);
  unused(ff);
  
  assoc = (mps_addr_t *)p[0];

  vword = w[WV];
  vf = vword & 7;
  vl = (mps_word_t)p[fl] >> 2;

  /* weak vectors should be non-stretchy traceable */
  assert(vf == 2);
  unused(vf);
  
  /* q is end of the object.  There are fl fixed fields, vl variable */
  /* fields and another slot that contains the vector length */
  q = p + fl + vl + 1;

  res = dylan_scan_contig_weak(mps_ss, p, q, base, assoc);
  if(res != MPS_RES_OK) {
    return res;
  }

  *object_io = q;
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

  assert(base == limit);

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

  p = (mps_addr_t *)object;
  assert(p != NULL);

  h = (mps_word_t)p[0];         /* load the header word */

  /* If the object is forwarded, simply skip it. */
  if(h & 3) {
    mps_addr_t l;

    if((h & 3) == 1)            /* single-word */
      l = (mps_addr_t)(p + 1);
    else {                      /* multi-word */
      assert((h & 3) == 2);
      l = (mps_addr_t)p[1];
    }

    return l;
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

  return (mps_addr_t)p;
}

static void dylan_copy(mps_addr_t old, mps_addr_t new)
{
  char *base = (char *)old;
  char *limit = (char *)dylan_skip(old);
  size_t length;
  assert(base < limit);
  length = (size_t)(limit - base);
  assert(dylan_wrapper_check(*(mps_word_t **)old));
  /* .improve.memcpy: Can do better here as we know that new and old
     will be aligned (to MPS_PF_ALIGN) */
  (void)memcpy(new, old, length);
}

static mps_addr_t dylan_isfwd(mps_addr_t object)
{
  mps_word_t h, tag;

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

void dylan_pad(mps_addr_t addr, size_t size)
{
  mps_word_t *p;

  p = (mps_word_t *)addr;
  if(size == sizeof(mps_word_t))        /* single-word object? */
    p[0] = 1;
  else {
    p[0] = 2;
    p[1] = (mps_word_t)((char *)addr + size);
  }
}


/* The dylan format structures */

static struct mps_fmt_A_s dylan_fmt_A_s =
{
  ALIGN,
  dylan_scan,
  dylan_skip,
  dylan_copy,
  dylan_fwd,
  dylan_isfwd,
  dylan_pad
};

static struct mps_fmt_B_s dylan_fmt_B_s =
{
  ALIGN,
  dylan_scan,
  dylan_skip,
  dylan_copy,
  dylan_fwd,
  dylan_isfwd,
  dylan_pad,
  dylan_class
};

/* Functions returning the dylan format structures */

mps_fmt_A_s *dylan_fmt_A(void)
{
  return &dylan_fmt_A_s;
}

mps_fmt_B_s *dylan_fmt_B(void)
{
  return &dylan_fmt_B_s;
}

/* Format variety-independent version that picks the right format
 * variety and creates it.  */

mps_res_t dylan_fmt(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
  return mps_fmt_create_B(mps_fmt_o, arena, dylan_fmt_B());
}

/* The weak format structures */

static struct mps_fmt_A_s dylan_fmt_A_weak_s =
{
  ALIGN,
  dylan_scan_weak,
  dylan_skip,
  no_copy,
  no_fwd,
  no_isfwd,
  no_pad
};

static struct mps_fmt_B_s dylan_fmt_B_weak_s =
{
  ALIGN,
  dylan_scan_weak,
  dylan_skip,
  no_copy,
  no_fwd,
  no_isfwd,
  no_pad,
  dylan_class
};

/* Functions returning the weak format structures */

mps_fmt_A_s *dylan_fmt_A_weak(void)
{
  return &dylan_fmt_A_weak_s;
}


mps_fmt_B_s *dylan_fmt_B_weak(void)
{
  return &dylan_fmt_B_weak_s;
}


/* Format variety-independent version that picks the right format
 * variety and creates it.  */

mps_res_t dylan_fmt_weak(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
  return mps_fmt_create_B(mps_fmt_o, arena, dylan_fmt_B_weak());
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
