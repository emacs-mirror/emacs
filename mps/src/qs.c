/*  impl.c.qs:                QUICKSORT
 *
 *  $HopeName: MMsrc!qs.c(trunk.8) $
 *
 *  Copyright (C) 1995,1996 Harlequin Group, all rights reserved
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
 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpscmv.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t skip(mps_addr_t object);
static void move(mps_addr_t object, mps_addr_t to);
static mps_addr_t isMoved(mps_addr_t object);
static void copy(mps_addr_t object, mps_addr_t to);
static void pad(mps_addr_t base, size_t size);

struct mps_fmt_A_s fmt_A_s =
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
  mps_word_t value;
  QSCell tail;
} QSCellStruct;


static mps_space_t space;
static mps_fmt_t format;
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
static mps_word_t reg[NREGS];
static mps_word_t regtag[NREGS];


/*  Machine Instructions
 *
 *  The machine can perform the following operations:
 *  cons
 *  append
 *  print
 *  swap
 */

/* should cons return in reg[0] or should it return via C? */
static
void
cons(mps_word_t tag0, mps_word_t value0, QSCell tail)
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

  reg[0] = (mps_word_t)new;
  regtag[0] = QSRef;
  return;
}

/* Appends reg[1] to reg[0] */
/* append nil, y = y
 * append x::xs, y = x::append xs, y
 * append x,y = (if (null x) y (cons (car x) (append (cdr x) y)))
 */
static
void
append(void)
{
  assert(regtag[0] == QSRef);
  assert(regtag[1] == QSRef);

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
  assert(regtag[0] == QSRef);
  reg[0] = (mps_word_t)((QSCell)reg[0])->tail; /* (cdr x) */
  regtag[0] = QSRef;
  append();
  reg[1] = reg[0];
  regtag[1] = regtag[0];
  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  assert(regtag[0] == QSRef);
  regtag[0] = ((QSCell)reg[0])->tag;
  reg[0] = ((QSCell)reg[0])->value; /* (car x) */
  cons(regtag[0], reg[0], (QSCell)reg[1]);
  activationStack = activationStack->tail->tail;

  ret:
  /* null out reg[1] */
  regtag[1] = QSRef;
  reg[1] = (mps_word_t)0;
  return;
}


static
void
print(QSCell a, FILE *stream)
{
  fprintf(stream, "( ");

  while(a != NULL) {
    switch(a->tag) {
    case QSInt:
      fprintf(stream, "%lu ", a->value);
      break;
    case QSRef:
      fprintf(stream, "<%08lx> ", a->value);
      break;
    default:
      fprintf(stream, "? ");
      break;
    }
    a = a->tail;
  }
  fprintf(stream, ")\n");
}

/* swaps reg[0] with reg[1], destroys reg[2] */
static
void
swap(void)
{
  regtag[2]=regtag[0];
  reg[2]=reg[0];
  regtag[0]=regtag[1];
  reg[0]=reg[1];
  regtag[1]=regtag[2];
  reg[1]=reg[2];
  regtag[2]=QSRef;
  reg[2]=(mps_word_t)0;
}

static
void
makerndlist(int l)
{
  int i;
  mps_word_t r;

  assert(l > 0);
  if(list != NULL) {
    mps_free(mpool, (mps_addr_t)list, (listl * sizeof(mps_word_t)));
    list = NULL;
  }
  listl = l;
  die(mps_alloc((mps_addr_t *)&list, mpool, (l * sizeof(mps_word_t))),
      "Alloc List");
  reg[0] = (mps_word_t)0;
  regtag[0] = QSRef;
  for(i = 0; i < l; ++i) {
    r = rnd();
    cons(QSInt, r, (QSCell)reg[0]);
    list[i] = r;
  }
}

/* reg[0] is split into two lists: those elements less than p, and
 * those elements >= p.  The two lists are returned in reg[0] and reg[1]
 */
static
void
part(mps_word_t p)
{
  regtag[2]=regtag[0];
  reg[2]=reg[0];
  assert(regtag[2] == QSRef);
  regtag[0]=QSRef;
  reg[0]=(mps_word_t)0;
  regtag[1]=QSRef;
  reg[1]=(mps_word_t)0;

  while(reg[2] != (mps_word_t)0) {
    assert(((QSCell)reg[2])->tag == QSInt);
    if(((QSCell)reg[2])->value < p) {
      /* cons onto reg[0] */
      cons(QSInt, ((QSCell)reg[2])->value, (QSCell)reg[0]);
    } else {
      /* cons onto reg[1] */
      cons(QSRef, (mps_word_t)reg[0], activationStack); /* save reg0 */
      activationStack = (QSCell)reg[0];
      cons(QSInt, ((QSCell)reg[2])->value, (QSCell)reg[1]);
      reg[1]=reg[0];
      reg[0]=activationStack->value;
      activationStack = activationStack->tail;
    }
    reg[2]=(mps_word_t)((QSCell)reg[2])->tail;
  }
}

/* applies the quicksort algorithm to sort reg[0] */
static
void
qs(void)
{
  mps_word_t pivot;

  assert(regtag[0] == QSRef);

  /* base case */
  if(reg[0] == (mps_word_t)0) {
    return;
  }

  /* check that we have an int list */
  assert(((QSCell)reg[0])->tag == QSInt);

  pivot = ((QSCell)reg[0])->value;
  reg[0] = (mps_word_t)((QSCell)reg[0])->tail;
  part(pivot);

  cons(QSRef, reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  cons(QSRef, reg[1], activationStack);
  activationStack = (QSCell)reg[0];

  reg[0] = reg[1];
  regtag[0] = regtag[1];
  assert(regtag[0] == QSRef);
  qs();
  cons(QSInt, pivot, (QSCell)reg[0]);
  activationStack = activationStack->tail;
  cons(QSRef, (mps_word_t)reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  assert(regtag[0] == QSRef);
  qs();
  reg[1] = activationStack->value;
  regtag[1] = activationStack->tag;
  activationStack = activationStack->tail->tail;
  append();
}

static
void
printlist(FILE *stream)
{
  mps_word_t i;

  fprintf(stream, "[ ");
  for(i = 0; i < listl; ++i) {
    fprintf(stream, "%lu ", list[i]);
  }
  fprintf(stream, "]\n");
}


/*  Compare
 *
 *  Used as an argument to qsort()
 */
static
int
compare(const void *a, const void *b)
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
static
void
validate(void)
{
  mps_word_t i;

  assert(regtag[0] == QSRef);
  regtag[1] = regtag[0];
  reg[1] = reg[0];
  for(i = 0; i < listl; ++i) {
    assert(((QSCell)reg[1])->tag == QSInt);
    if(((QSCell)reg[1])->value != list[i]) {
      fprintf(stdout,
              "mps_res_t: Element %lu of the two lists do not match.\n",
              (unsigned long)i);
      return;
    }
    reg[1] = (mps_word_t)((QSCell)reg[1])->tail;
  }
  assert(reg[1] == (mps_word_t)0);
  fprintf(stdout, "Note: Lists compare equal.\n");
}


static
void *
go(void *p, size_t s)
{
  UNUSED(p);
  UNUSED(s);

  die(mps_pool_create(&mpool, space, mps_class_mv(),
                      (size_t)65536, sizeof(QSCellStruct) * 1000,
                      (size_t)65536), "MVCreate");

  die(mps_fmt_create_A(&format, space, &fmt_A_s), "FormatCreate");
  die(mps_pool_create(&pool, space, mps_class_amc(), format),
      "AMCCreate");
  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "APCreate");
  die(mps_root_create_table(&regroot, space, MPS_RANK_AMBIG, 0,
      (mps_addr_t *)reg, NREGS), "RootCreateTable");
  die(mps_root_create_table(&actroot, space, MPS_RANK_AMBIG, 0,
      (mps_addr_t *)&activationStack, sizeof(QSCell)/sizeof(mps_addr_t)),
      "RootCreateTable");

  /* makes a random list */
  makerndlist(1000);
  print((QSCell)reg[0], stdout);

  part(0);
  swap();
  qs();
  print((QSCell)reg[0], stdout);
  printlist(stdout);
  qsort(list, listl, sizeof(mps_word_t), &compare);
  printlist(stdout);
  validate();

  mps_root_destroy(regroot);
  mps_root_destroy(actroot);
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_pool_destroy(mpool);
  mps_fmt_destroy(format);

  return NULL;
}

int
main(void)
{
  void *r;
  die(mps_space_create(&space), "SpaceCreate");
  mps_tramp(&r, &go, NULL, 0);
  mps_space_destroy(space);

  return 0;
}

/*  Machine Object Format  */

static
void
pad(mps_addr_t base, size_t size)
{
  mps_word_t *object = base;
  assert(size >= sizeof(mps_word_t));
  if(size == sizeof(mps_word_t)) {
    object[0] = QSPadOne;
    return;
  }
  assert(size >= 2*sizeof(mps_word_t));
  object[0] = QSPadMany;
  object[1] = size;
  return;
}

static
mps_res_t
scan1(mps_ss_t ss, mps_addr_t *objectIO)
{
  QSCell cell;
  mps_res_t res;

  assert(objectIO != NULL);

  MPS_SCAN_BEGIN(ss) {
    cell = (QSCell)*objectIO;

    switch(cell->tag) {
    case QSRef:
      if(!MPS_FIX1(ss, (mps_addr_t)cell->value))
        goto fixTail;
      res = MPS_FIX2(ss, (mps_addr_t *)&cell->value);
      if(res != MPS_RES_OK)
        return res;
    /* fall */

    case QSInt:
    fixTail:
      if(!MPS_FIX1(ss, (mps_addr_t)cell->tail))
        break;
      res = MPS_FIX2(ss, (mps_addr_t *)&cell->tail);
      if(res != MPS_RES_OK)
        return res;
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
      assert(0);
      return MPS_RES_OK;
    }
  } MPS_SCAN_END(ss);

  *objectIO = (mps_addr_t)(cell+1);

  return MPS_RES_OK;
}

static
mps_res_t
scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{

  while(base < limit) {
    mps_res_t res;

    res = scan1(ss, &base);
    if(res != MPS_RES_OK) {
      return res;
    }
  }

  assert(base == limit);

  return MPS_RES_OK;
}

static
mps_addr_t
skip(mps_addr_t object)
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

static
void
move(mps_addr_t object, mps_addr_t to)
{
  QSCell cell;

  cell = (QSCell)object;

  cell->tag = QSEvac;
  cell->value = (mps_word_t)to;
}

static
mps_addr_t
isMoved(mps_addr_t object)
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
