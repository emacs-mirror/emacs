/* impl.c.fmtdy: DYLAN OBJECT FORMAT IMPLEMENTATION
 *
 *  $HopeName: MMsrc!fmtdy.c(MMdevel_restr.2) $
 *  Copyright (C) 1996 Harlequin Group, all rights reserved.
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
 *  W+2         (FL << 2) | FF  fixed part length and format
 *  W+3         (VS << 3) | VF  variable part format and element size
 *  W+4         (WT << 2) | 1   tagged pattern vector length
 *  W+5         pattern 0       patterns for fixed part fields
 *  W+5+WT-1    pattern WT-1
 *
 *  The wrapper wrapper, WW:
 *
 *  WW          WW              WW is it's own wrapper
 *  WW+1        class           DylanWorks class of wrappers
 *  WW+2        (3 << 2) | 2    wrappers have three patterned fields
 *  WW+3        (0 << 3) | 0    wrappers have a non-traceable vector
 *  WW+4        (1 << 2) | 1    one pattern word follows
 *  WW+5        0b001           only field 0 is traceable
 */

#include "fmtdy.h"
#include "mps.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define notreached()    do assert(0); while(0)

#define ALIGN           sizeof(mps_word_t)

static int dylan_wrapper_check(mps_word_t *w)
{
  mps_word_t *ww;
  mps_word_t class;
  mps_word_t fh, fl, ff;
  mps_word_t vh, vs, vf;
  mps_word_t vt, t;

  assert(w != NULL);
  assert(((mps_word_t)w & 3) == 0);

  /* The first word of the wrapper is a pointer to a wrapper wrapper, */
  /* which always has the same contents.  Check it. */

  /* @@@@ When this becomes part of the Dylan run-time, it would be */
  /* possible to know the address of a unique wrapper wrapper and */
  /* check that instead. */

  assert(w[0] != 0);
  assert((w[0] & 3) == 0);          /* wrapper wrapper is aligned */
  ww = (mps_word_t *)w[0];
  assert(ww[0] == w[0]);            /* wrapper wrapper is own wrapper */
  assert(ww[1] != 0);               /* wrapper class exists */
  assert((ww[1] & 3) == 0);         /* wrapper class is aligned */
  assert(ww[2] == ((3 << 2) | 2));  /* three fields with patterns */
  assert(ww[3] == ((0 << 3) | 0));  /* non-traceable vector of pats */
  assert(ww[4] == ((1 << 2) | 1));  /* one pattern word in wrapper */
  assert(ww[5] == 1);               /* first field traceable */

  /* Unpack the wrapper. */

  class = w[1];         /* class */
  fh = w[2];            /* fixed part header word */
  fl = fh >> 2;         /* fixed part length */
  ff = fh & 3;          /* fixed part format code */
  vh = w[3];            /* variable part header */
  vs = vh >> 3;         /* variable part length */
  vf = vh & 7;          /* variable part format code */
  vt = w[4];            /* vector total word (Dylan-tagged) */
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
  assert(vh != 6);

  /* There should be no shift in word vector formats. */
  assert((vf & 6) == 4 || vs == 0);

  /* @@@@ Dylan only uses byte vectors, so the shift should be 3. */
  /* Note that the code supports other sizes -- this check is only */
  /* for Dylan consistency. */
  assert((vf & 6) != 4 || vs == 3);

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

  assert(ff != 2 || (w[4+t] >> ((fh>>2) & (MPS_WORD_WIDTH-1))) == 0);

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

  MPS_SCAN_BEGIN(mps_ss) {
          p = base;
          goto in;
    pat:  p += MPS_WORD_WIDTH;
          if(p >= limit) goto out;
    in:   pp = p;
          pat = *pc++;
    loop: if(pat == 0) goto pat;
          ++pp;
          b = pat & 1;
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

#define NONWORD_LENGTH(_vt, _vs) \
  ((_vs) < MPS_WORD_SHIFT ? \
   ((_vt) + (1 << (MPS_WORD_SHIFT - (_vs))) - 1) >> \
     (MPS_WORD_SHIFT - (_vs)) : \
   (_vt) << ((_vs) - MPS_WORD_SHIFT))

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
  unsigned vs;          /* variable part element size (log2 of bits) */
  mps_word_t vt;        /* total vector length */
  mps_res_t res;

  assert(object_io != NULL);

  p = (mps_addr_t *)*object_io;
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

    *object_io = l;
    return MPS_RES_OK;
  }

  mps_fix(mps_ss, p);           /* fix the wrapper */
  w = (mps_word_t *)p[0];       /* wrapper is header word */
  assert(dylan_wrapper_check(w));

  ++p;                          /* skip header */

  /* Fixed Part */

  fh = w[2];
  fl = fh >> 2;                 /* get the fixed part length */

  /* It might be worth inlining common cases here, for example, */
  /* pairs.  This can be done by examining fh as a whole. */

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
      res = dylan_scan_pat(mps_ss, p, q, &w[5], w[4]>>2);
      if(res) return res;
      break;

      default:
      notreached();
      break;
    }
    p = q;
  }

  /* Variable Part */
  vh = w[3];
  vf = vh & 7;                  /* get variable part format */
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
      vs = vh >> 3;
      p += NONWORD_LENGTH(vt, vs);
      break;

      case 5:                   /* stretchy non-word */
      notreached();             /* DW doesn't create them yet */
      vs = vh >> 3;
      p += NONWORD_LENGTH(vt, vs) + 1;
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

static mps_addr_t dylan_skip(mps_addr_t object)
{
  mps_addr_t *p;        /* cursor in object */
  mps_word_t *w;        /* wrapper cursor */
  mps_word_t h;         /* header word */
  mps_word_t vh;        /* variable part header */
  mps_word_t vf;        /* variable part format */
  mps_word_t vt;        /* total vector length */
  unsigned vs;          /* variable part element size (log2 of bits) */

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

  p += w[2] >> 2;               /* skip fixed part fields */

  vf = w[3] & 7;                /* get variable part format */
  if(vf != 7)
  {
    vh = *(mps_word_t *)p;
    assert((vh & 3) == 1);      /* check Dylan integer tag */
    vt = vh >> 2;               /* total length */
    ++p;

    p += vf & 1;                /* stretchy vectors have an extra word */

    if((vf & 6) == 4)           /* non-word */
    {
      vs = w[3] >> 3;
      p += NONWORD_LENGTH(vt, vs);
    }
    else
      p += vt;
  }

  return (mps_addr_t)p;
}

static void dylan_copy(mps_addr_t old, mps_addr_t new)
{
  size_t length = (char *)dylan_skip(old) - (char *)old;
  assert(dylan_wrapper_check(*(mps_word_t **)old));
  memcpy(new, old, length);
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

static void dylan_pad(mps_addr_t addr, size_t size)
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

mps_fmt_A_s *dylan_fmt_A(void)
{
  return &dylan_fmt_A_s;
}

/* Format Test Code */

static mps_word_t *ww = NULL;
static mps_word_t *tvw;

mps_res_t dylan_init(mps_addr_t addr, size_t size,
                     mps_addr_t *refs, size_t nr_refs)
{

  /* Make sure the size is aligned. */
  assert((size & (ALIGN-1)) == 0);

  if(ww == NULL) {
    ww = malloc(sizeof(mps_word_t) * 6);
    if(ww == NULL) return MPS_RES_MEMORY;
    tvw = malloc(sizeof(mps_word_t) * 5);
    if(tvw == NULL) {
      free(ww);
      return MPS_RES_MEMORY;
    }

    /* Build a wrapper wrapper. */
    ww[0] = (mps_word_t)ww;
    ww[1] = (mps_word_t)ww;     /* dummy class */
    ww[2] = (3 << 2) | 2;
    ww[3] = (0 << 3) | 0;
    ww[4] = (1 << 2) | 1;
    ww[5] = 1;

    /* Build a wrapper for traceable vectors. */
    tvw[0] = (mps_word_t)ww;
    tvw[1] = (mps_word_t)ww;    /* dummy class */
    tvw[2] = 0;                 /* no fixed part */
    tvw[3] = 2;                 /* traceable variable part */
    tvw[4] = 1;                 /* no patterns */
  }

  /* If there is enough room, make a vector, otherwise just */
  /* make a padding object. */

  if(size >= sizeof(mps_word_t) * 2) {
    mps_word_t *p = (mps_word_t *)addr;
    mps_word_t i, t = (size / sizeof(mps_word_t)) - 2;
    p[0] = (mps_word_t)tvw;     /* install vector wrapper */
    p[1] = (t << 2) | 1;        /* tag the vector length */
    for(i = 0; i < t; ++i)
      p[2+i] = (mps_word_t)refs[rand() % nr_refs];
  } else
    dylan_pad(addr, size);

  return MPS_RES_OK;
}

void dylan_write(mps_addr_t addr, mps_addr_t *refs, size_t nr_refs)
{
  mps_word_t *p = (mps_word_t *)addr;

  /* If the object is a vector, update a random entry. */
  if(p[0] == (mps_word_t)tvw) {
    mps_word_t t = p[1] >> 2;
    if(t > 0)
      p[2 + (rand() % t)] =
        (mps_word_t)refs[rand() % nr_refs];
  }
}

mps_addr_t dylan_read(mps_addr_t addr)
{
  mps_word_t *p = (mps_word_t *)addr;

  /* If the object is a vector, return a random entry. */
  if(p[0] == (mps_word_t)tvw) {
    mps_word_t t = p[1] >> 2;
    if(t > 0)
      return (mps_addr_t)p[2 + (rand() % t)];
  }

  return addr;
}

mps_bool_t dylan_check(mps_addr_t addr)
{
  mps_word_t *p = (mps_word_t *)addr;
  assert(addr != 0);
  assert(((mps_word_t)addr & (ALIGN-1)) == 0);
  assert(dylan_wrapper_check((mps_word_t *)p[0]));
  return 1;
}
