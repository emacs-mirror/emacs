/* qs.c: QUICKSORT
 *
 *  $Id$
 *  Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 *  The purpose of this program is to act as a "real" client of the MM.
 *  It is a test, but (hopefully) less contrived than some of the other
 *  tests.
 *
 *  C stack will contain the continuations (list of PCs).  The
 *  activation stack will parallel the C stack and contain the program's
 *  variables.  This is all slightly bizarre.
 *  And qs cheats a tiny bit by using the C stack to save leaf objects
 *  (integers).
 *
 *  nil, the end of list, is represented by a NULL pointer.
 *
 *  list length 1000 makes 40404 conses (by experiment).
 *
 *  Some registers are not nulled out when they could be.
 *
 *  TODO: There should be fewer casts and more unions.
 */

#include "testlib.h"
#include "mpslib.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscmvff.h"
#include "mpstd.h"

#include <stdio.h> /* printf */
#include <stdlib.h> /* qsort */


#define testArenaSIZE ((size_t)1000*1024)
#define genCOUNT 2

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
  { 150, 0.85 }, { 170, 0.45 } };


static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t skip(mps_addr_t object);
static void move(mps_addr_t object, mps_addr_t to);
static mps_addr_t isMoved(mps_addr_t object);
static void copy(mps_addr_t object, mps_addr_t to);
static void pad(mps_addr_t base, size_t size);

static struct mps_fmt_A_s fmt_A_s =
  {
    (mps_align_t)4,
    scan, skip, copy,
    move, isMoved,
    pad
  };



/* Tags used by object format */
enum {QSInt, QSRef, QSEvac, QSPadOne, QSPadMany};

typedef struct QSCellStruct *QSCell;
typedef struct QSCellStruct {
  mps_word_t tag;
  mps_addr_t value;
  QSCell tail;
} QSCellStruct;


static mps_arena_t arena;
static mps_pool_t pool;     /* automatic pool */
static mps_ap_t ap;         /* AP for above */
static mps_pool_t mpool;    /* manual pool */
static mps_root_t regroot;
static mps_root_t actroot;


/*  list holds an array that we qsort(), listl is its length */
static mps_word_t *list;
static mps_word_t listl;


/*  Machine State
 *
 *  The machine consists of a stack and 3 registers.
 */

static QSCell activationStack;
#define NREGS 3
static mps_addr_t reg[NREGS];
static mps_word_t regtag[NREGS];


/*  Machine Instructions
 *
 *  The machine can perform the following operations:
 *  cons
 *  append
 *  swap
 */

/* should cons return in reg[0] or should it return via C? */
static void cons(mps_word_t tag0, mps_addr_t value0, QSCell tail)
{
  mps_addr_t p;
  QSCell new;

  do {
    die(mps_reserve(&p, ap, sizeof(QSCellStruct)),
        "cons");
    new = (QSCell)p;
    new->tag = tag0;
    new->value = value0;
    new->tail = tail;
  } while(!mps_commit(ap, p, sizeof(QSCellStruct)));

  reg[0] = (mps_addr_t)new;
  regtag[0] = QSRef;
}


/* Appends reg[1] to reg[0] */
/* append nil, y = y
 * append x::xs, y = x::append xs, y
 * append x,y = (if (null x) y (cons (car x) (append (cdr x) y)))
 */
static void append(void)
{
  cdie(regtag[0] == QSRef, "append 0");
  cdie(regtag[1] == QSRef, "append 1");

  if(reg[0] == (mps_word_t)0) {
    reg[0] = reg[1];
    regtag[0] = regtag[1];
    goto ret;
  }

  cons(regtag[0], reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  cons(regtag[1], reg[1], activationStack);
  activationStack = (QSCell)reg[0];

  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  cdie(regtag[0] == QSRef, "append tail");
  reg[0] = (mps_addr_t)((QSCell)reg[0])->tail; /* (cdr x) */
  regtag[0] = QSRef;
  append();
  reg[1] = reg[0];
  regtag[1] = regtag[0];
  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  cdie(regtag[0] == QSRef, "append sec");
  regtag[0] = ((QSCell)reg[0])->tag;
  reg[0] = ((QSCell)reg[0])->value; /* (car x) */
  cons(regtag[0], reg[0], (QSCell)reg[1]);
  activationStack = activationStack->tail->tail;

  ret:
  /* null out reg[1] */
  regtag[1] = QSRef;
  reg[1] = (mps_addr_t)0;
}


/* swaps reg[0] with reg[1], destroys reg[2] */
static void swap(void)
{
  regtag[2]=regtag[0];
  reg[2]=reg[0];
  regtag[0]=regtag[1];
  reg[0]=reg[1];
  regtag[1]=regtag[2];
  reg[1]=reg[2];
  regtag[2]=QSRef;
  reg[2]=(mps_addr_t)0;
}


static void makerndlist(unsigned l)
{
  size_t i;
  mps_word_t r;
  mps_addr_t addr;

  cdie(l > 0, "list len");
  if(list != NULL) {
    mps_free(mpool, (mps_addr_t)list, (listl * sizeof(mps_word_t)));
    list = NULL;
  }
  listl = l;
  addr = list;
  die(mps_alloc(&addr, mpool, (l * sizeof(mps_word_t))),
      "Alloc List");
  list = addr;
  reg[0] = (mps_addr_t)0;
  regtag[0] = QSRef;
  for(i = 0; i < l; ++i) {
    r = rnd();
    cons(QSInt,
         (mps_addr_t)r, /* TODO: dirty cast */
         (QSCell)reg[0]);
    list[i] = r;
  }
}


/* reg[0] is split into two lists: those elements less than p, and
 * those elements >= p.  The two lists are returned in reg[0] and reg[1]
 */
static void part(mps_word_t p)
{
  regtag[2]=regtag[0];
  reg[2]=reg[0];
  cdie(regtag[2] == QSRef, "part 0");
  regtag[0]=QSRef;
  reg[0]=(mps_addr_t)0;
  regtag[1]=QSRef;
  reg[1]=(mps_addr_t)0;

  while(reg[2] != (mps_word_t)0) {
    cdie(((QSCell)reg[2])->tag == QSInt, "part int");
    if((mps_word_t)((QSCell)reg[2])->value < p) {
      /* cons onto reg[0] */
      cons(QSInt, ((QSCell)reg[2])->value, (QSCell)reg[0]);
    } else {
      /* cons onto reg[1] */
      cons(QSRef, reg[0], activationStack); /* save reg0 */
      activationStack = (QSCell)reg[0];
      cons(QSInt, ((QSCell)reg[2])->value, (QSCell)reg[1]);
      reg[1]=reg[0];
      reg[0]=activationStack->value;
      activationStack = activationStack->tail;
    }
    reg[2]=(mps_addr_t)((QSCell)reg[2])->tail;
  }
}


/* applies the quicksort algorithm to sort reg[0] */
static void qs(void)
{
  mps_word_t pivot;

  cdie(regtag[0] == QSRef, "qs 0");

  /* base case */
  if(reg[0] == (mps_word_t)0) {
    return;
  }

  /* check that we have an int list */
  cdie(((QSCell)reg[0])->tag == QSInt, "qs int");

  pivot = (mps_word_t)((QSCell)reg[0])->value;
  reg[0] = (mps_addr_t)((QSCell)reg[0])->tail;
  part(pivot);

  cons(QSRef, reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  cons(QSRef, reg[1], activationStack);
  activationStack = (QSCell)reg[0];

  reg[0] = reg[1];
  regtag[0] = regtag[1];
  cdie(regtag[0] == QSRef, "qs 1");
  qs();
  cons(QSInt, (mps_addr_t)pivot, (QSCell)reg[0]);
  activationStack = activationStack->tail;
  cons(QSRef, reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  cdie(regtag[0] == QSRef, "qs tail");
  qs();
  reg[1] = activationStack->value;
  regtag[1] = activationStack->tag;
  activationStack = activationStack->tail->tail;
  append();
}


/*  Compare
 *
 *  Used as an argument to qsort()
 */
static int compare(const void *a, const void *b)
{
  mps_word_t aa, bb;

  aa = *(const mps_word_t *)a;
  bb = *(const mps_word_t *)b;
  if(aa < bb) {
    return -1;
  } else if(aa == bb) {
    return 0;
  } else {
    return 1;
  }
}


/*  compares the qsort'ed list with our quicksorted list  */
static void validate(void)
{
  mps_word_t i;

  cdie(regtag[0] == QSRef, "validate 0");
  regtag[1] = regtag[0];
  reg[1] = reg[0];
  for(i = 0; i < listl; ++i) {
    cdie(((QSCell)reg[1])->tag == QSInt, "validate int");
    if((mps_word_t)((QSCell)reg[1])->value != list[i]) {
        printf("mps_res_t: Element %"PRIuLONGEST" of the "
               "two lists do not match.\n", (ulongest_t)i);
      return;
    }
    reg[1] = (mps_addr_t)((QSCell)reg[1])->tail;
  }
  cdie(reg[1] == (mps_word_t)0, "validate end");
  printf("Note: Lists compare equal.\n");
}


static void *go(void *p, size_t s)
{
  mps_fmt_t format;
  mps_chain_t chain;
  mps_addr_t base;

  testlib_unused(p);
  testlib_unused(s);

  die(mps_pool_create_k(&mpool, arena, mps_class_mvff(), mps_args_none),
      "pool create");

  die(mps_fmt_create_A(&format, arena, &fmt_A_s), "FormatCreate");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
      "AMCCreate");
  die(mps_ap_create(&ap, pool, mps_rank_exact()), "APCreate");
  die(mps_root_create_table(&regroot,
                            arena,
                            mps_rank_ambig(),
                            0,
                            reg,
                            NREGS),
      "RootCreateTable");

  base = &activationStack;
  die(mps_root_create_table(&actroot, arena, mps_rank_ambig(), 0, base, 1),
      "RootCreateTable");

  /* makes a random list */
  makerndlist(1000);

  part(0);
  swap();
  qs();
  qsort(list, listl, sizeof(mps_word_t), &compare);
  validate();

  mps_arena_park(arena);
  mps_root_destroy(regroot);
  mps_root_destroy(actroot);
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_pool_destroy(mpool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_arena_release(arena);

  return NULL;
}


/*  Machine Object Format  */

static void pad(mps_addr_t base, size_t size)
{
  mps_word_t *object = base;
  cdie(size >= sizeof(mps_word_t), "pad size");
  if(size == sizeof(mps_word_t)) {
    object[0] = QSPadOne;
    return;
  }
  cdie(size >= 2*sizeof(mps_word_t), "pad size 2");
  object[0] = QSPadMany;
  object[1] = size;
}


static mps_res_t scan1(mps_ss_t ss, mps_addr_t *objectIO)
{
  QSCell cell;
  mps_res_t res;
  mps_addr_t addr;

  cdie(objectIO != NULL, "objectIO");

  MPS_SCAN_BEGIN(ss) {
    cell = (QSCell)*objectIO;

    switch(cell->tag) {
    case QSRef:
      addr = cell->value;
      if(!MPS_FIX1(ss, addr))
        goto fixTail;
      res = MPS_FIX2(ss, &addr);
      if(res != MPS_RES_OK)
        return res;
      cell->value = addr;
    /* fall through */

    case QSInt:
    fixTail:
      addr = cell->tail;
      if(!MPS_FIX1(ss, addr))
        break;
      res = MPS_FIX2(ss, &addr);
      if(res != MPS_RES_OK)
        return res;
      cell->tail = addr;
      break;

    case QSEvac:
      /* skip */
      break;

    case QSPadOne:
      *objectIO = (mps_addr_t)((mps_word_t *)cell+1);
      return MPS_RES_OK;

    case QSPadMany:
      *objectIO = (mps_addr_t)((mps_word_t)cell+((mps_word_t *)cell)[1]);
      return MPS_RES_OK;

    default:
      cdie(0, "unknown tag");
      return MPS_RES_OK;
    }
  } MPS_SCAN_END(ss);

  *objectIO = (mps_addr_t)(cell+1);

  return MPS_RES_OK;
}


static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  Insist(mps_arena_busy(arena));
  while(base < limit) {
    mps_res_t res;

    res = scan1(ss, &base);
    if(res != MPS_RES_OK) {
      return res;
    }
  }

  cdie(base == limit, "scan limit");
  return MPS_RES_OK;
}


static mps_addr_t skip(mps_addr_t object)
{
  QSCell cell = (QSCell)object;
  switch(cell->tag)
  {
  case QSPadOne:
    return (mps_addr_t)((mps_word_t *)cell+1);
  case QSPadMany:
    return (mps_addr_t)((mps_word_t)cell+((mps_word_t *)cell)[1]);
  default:
    return (mps_addr_t)((QSCell)object + 1);
  }
}


static void move(mps_addr_t object, mps_addr_t to)
{
  QSCell cell;

  cell = (QSCell)object;

  cell->tag = QSEvac;
  cell->value = to;
}


static mps_addr_t isMoved(mps_addr_t object)
{
  QSCell cell;

  cell = (QSCell)object;

  if(cell->tag == QSEvac) {
    return (mps_addr_t)cell->value;
  }
  return (mps_addr_t)0;
}


static void copy(mps_addr_t object, mps_addr_t to)
{
  QSCell cells, celld;

  cells = (QSCell)object;
  celld = (QSCell)to;

  *celld = *cells;
}


int main(int argc, char *argv[])
{
  void *r;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");

  mps_tramp(&r, &go, NULL, 0);
  mps_arena_destroy(arena);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
