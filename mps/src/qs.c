/*  impl.c.qs
 *
 *                          QUICKSORT
 *
 *  $HopeName$
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
 *  .collect.hack: There's a hack to start a collection.  It's
 *    completely unsatisfactory, because AMC doesn't collect
 *    on its own yet.
 */


#include "std.h"
#include "lib.h"
#include "amc.h"
#include "coll.h"
#include "format.h"
#include "pool.h"
#include "root.h"
#include "space.h"
#include "testlib.h"

#include <stdlib.h>


static Error scan(ScanState ss, Addr base, Addr limit);
static Addr skip(Addr object);
static void move(Addr object, Addr to);
static Addr isMoved(Addr object);
static void copy(Addr object, Addr to);


/* Tags used by object format */
enum {QSInt, QSRef, QSEvac};

typedef struct QSCellStruct *QSCell;
typedef struct QSCellStruct
{
  Addr tag;
  Addr value;
  QSCell tail;
} QSCellStruct;


static Space space;
static Format format;
static Pool pool;        /* automatic pool */
static Buffer buffer;    /* buffer for above */
static Pool mpool;       /* manual pool */

/*  list holds an array that we qsort(), listl is its length */
static Addr *list;
static Addr listl;

/*  Machine State
 *
 *  The machine consists of a stack and 3 registers.
 */

static QSCell activationStack;
#define NREGS 3
static Addr reg[NREGS];
static Addr regtag[NREGS];


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
cons(Addr tag0, Addr value0, QSCell tail)
{
  Addr p;
  QSCell new;

  do {
    die(BufferReserve(&p, buffer, (Size)sizeof(QSCellStruct)),
	"cons");
    new = (QSCell)p;
    new->tag = tag0;
    new->value = value0;
    new->tail = tail;
  } while(!BufferCommit(buffer, p, (Size)sizeof(QSCellStruct)));

  reg[0] = (Addr)new;
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
  AVER(regtag[0] == QSRef);
  AVER(regtag[1] == QSRef);

  if(reg[0] == (Addr)0) {
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
  AVER(regtag[0] == QSRef);
  reg[0] = (Addr)((QSCell)reg[0])->tail; /* (cdr x) */
  regtag[0] = QSRef;
  append();
  reg[1] = reg[0];
  regtag[1] = regtag[0];
  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  AVER(regtag[0] == QSRef);
  regtag[0] = ((QSCell)reg[0])->tag;
  reg[0] = ((QSCell)reg[0])->value; /* (car x) */
  cons(regtag[0], reg[0], (QSCell)reg[1]);
  activationStack = activationStack->tail->tail;

  ret:
  /* null out reg[1] */
  regtag[1] = QSRef;
  reg[1] = NULL;
  return;
}


static
void
print(QSCell a, LibStream stream)
{
  LibFormat(stream, "( ");

  while(a != NULL) {
    switch(a->tag) {
    case QSInt:
      LibFormat(stream, "%lu ", a->value);
      break;
    case QSRef:
      LibFormat(stream, "<%08lx> ", a->value);
      break;
    default:
      LibFormat(stream, "? ");
      break;
    }
    a = a->tail;
  }
  LibFormat(stream, ")\n");
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
  reg[2]=NULL;
}

static
void
makerndlist(int l)
{
  int i;
  Addr r;

  AVER(l > 0);
  if(list != NULL) {
    PoolFree(mpool, (Addr)list, (Size)(listl * sizeof(Addr)));
    list = NULL;
  }
  listl = l;
  die(PoolAlloc((Addr *)&list, mpool, (Size)(l * sizeof(Addr))),
      "Alloc List");
  reg[0] = NULL;
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
part(Addr p)
{
  regtag[2]=regtag[0];
  reg[2]=reg[0];
  AVER(regtag[2] == QSRef);
  regtag[0]=QSRef;
  reg[0]=NULL;
  regtag[1]=QSRef;
  reg[1]=NULL;

  while(reg[2] != NULL) {
    AVER(((QSCell)reg[2])->tag == QSInt);
    if(((QSCell)reg[2])->value < p) {
      /* cons onto reg[0] */
      cons(QSInt, ((QSCell)reg[2])->value, (QSCell)reg[0]);
    } else {
      /* cons onto reg[1] */
      cons(QSRef, (Addr)reg[0], activationStack); /* save reg[0] */
      activationStack = (QSCell)reg[0];
      cons(QSInt, ((QSCell)reg[2])->value, (QSCell)reg[1]);
      reg[1]=reg[0];
      reg[0]=activationStack->value;
      activationStack = activationStack->tail;
    }
    reg[2]=(Addr)((QSCell)reg[2])->tail;
  }
}

/* applies the quicksort algorithm to sort reg[0] */
static
void
qs(void)
{
  Addr pivot;

  AVER(regtag[0] == QSRef);

  /* base case */
  if(reg[0] == NULL) {
    return;
  }

  /* check that we have an int list */
  AVER(((QSCell)reg[0])->tag == QSInt);
   
  pivot = ((QSCell)reg[0])->value;
  reg[0] = (Addr)((QSCell)reg[0])->tail;
  part(pivot);

  cons(QSRef, reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  cons(QSRef, reg[1], activationStack);
  activationStack = (QSCell)reg[0];

  reg[0] = reg[1];
  regtag[0] = regtag[1];
  AVER(regtag[0] == QSRef);
  qs();
  cons(QSInt, pivot, (QSCell)reg[0]);
  activationStack = activationStack->tail;
  cons(QSRef, (Addr)reg[0], activationStack);
  activationStack = (QSCell)reg[0];
  reg[0] = activationStack->tail->value;
  regtag[0] = activationStack->tail->tag;
  AVER(regtag[0] == QSRef);
  qs();
  reg[1] = activationStack->value;
  regtag[1] = activationStack->tag;
  activationStack = activationStack->tail->tail;
  append();
}

static
void
printlist(LibStream stream)
{
  Addr i;

  LibFormat(stream, "[ ");
  for(i = 0; i < listl; ++i) {
    LibFormat(stream, "%lu ", list[i]);
  }
  LibFormat(stream, "]\n");
}


/*  Compare
 *
 *  Used as an argument to qsort()
 */
static
int
compare(const void *a, const void *b)
{
  Addr aa, bb;

  aa = *(const Addr *)a;
  bb = *(const Addr *)b;
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
  Addr i;

  AVER(regtag[0] == QSRef);
  regtag[1] = regtag[0];
  reg[1] = reg[0];
  for(i = 0; i < listl; ++i) {
    AVER(((QSCell)reg[1])->tag == QSInt);
    if(((QSCell)reg[1])->value != list[i]) {
      LibFormat(LibStreamOut(), 
		"Error: Element %lu of the two lists do not match.\n");
      return;
    }
    reg[1] = (Addr)((QSCell)reg[1])->tail;
  }
  AVER(reg[1] == NULL);
  LibFormat(LibStreamOut(), "Note: Lists compare equal.\n");
}


int
main(void)
{
  AMC amc;
  Coll coll;
  int i;

  die(SpaceCreate(&space), "SpaceCreate");
  mpool = SpaceControlPool(space);

  die(FormatCreate(&format, space, (Addr)4,
		   scan, skip,
		   move, isMoved,
		   copy), "FormatCreate");
  die(AMCCreate(&amc, space, format), "AMCCreate");
  pool = AMCPool(amc);
  die(BufferCreate(&buffer, pool), "BufferCreate");

  /* sanity checking */
  /* should print (1) (1) (1 1) (1 1 7) */

/* it works, so I'm commenting it out */
/*
  cons(QSInt, 1, NULL);
  print(reg[0], LibStreamOut());
  reg[1]=reg[0];
  regtag[1]=regtag[0];
  reg[0]=NULL;
  regtag[0]=QSRef;
  append();
  print(reg[0], LibStreamOut());
  reg[1]=reg[0];
  regtag[1]=regtag[0];
  append();
  print(reg[0], LibStreamOut());
  reg[1]=reg[0];
  regtag[1]=regtag[0];
  cons(QSInt, 7, NULL);
  swap();
  append();
  print(reg[0], LibStreamOut());
*/

  /* makes a random list */
  makerndlist(1000);
  print((QSCell)reg[0], LibStreamOut());

  part(0);
  swap();
  qs();
  print((QSCell)reg[0], LibStreamOut());
  printlist(LibStreamOut());
  qsort(list, listl, sizeof(Addr), &compare);
  printlist(LibStreamOut());
  validate();

  SpaceDescribe(space, LibStreamOut());

  die(CollCreate(&coll, pool), "CollCreate");
  die(SchedProcAdd(SpaceSched(space), CollProc, coll, 0),
      "SchedProcAdd");
  for(i = 0; i < 10; ++i) {
    SchedRun(SpaceSched(space));
  }

  SpaceDescribe(space, LibStreamOut());

  BufferDestroy(buffer);
  AMCDestroy(amc);
  FormatDestroy(format);
  SpaceDestroy(space);

  return 0;
}


/*  Machine Object Format  */

/* neither scan nor skip cope with forwarded objects */
static
Error
scan1(ScanState ss, Addr *objectIO)
{
  QSCell cell;
  Error e;

  AVER(objectIO != NULL);
  
  cell = (QSCell)*objectIO;

  switch(cell->tag) {
  case QSRef:
    e = TraceFix(ss, (Ref *)&cell->value);
    if(e != ErrSUCCESS)
      return e;
  /* fall */

  case QSInt:
    e = TraceFix(ss, (Ref *)&cell->tail);
    if(e != ErrSUCCESS)
      return e;
  break;

  case QSEvac:
  default:
    NOTREACHED;
    return ErrFAILURE;
  }
  *objectIO = (Addr)(cell+1);

  return ErrSUCCESS;
}

static
Error
scan(ScanState ss, Addr base, Addr limit)
{

  while(base < limit) {
    Error e;

    e = scan1(ss, &base);
    if(e != ErrSUCCESS) {
      return e;
    }
  }

  AVER(base == limit);

  return ErrSUCCESS;
}
 


static
Addr
skip(Addr object)
{
  return (Addr)((QSCell)object + 1);
}

static
void
move(Addr object, Addr to)
{
  QSCell cell;

  cell = (QSCell)object;

  cell->tag = QSEvac;
  cell->value = to;
}

static
Addr
isMoved(Addr object)
{
  QSCell cell;

  cell = (QSCell)object;

  if(cell->tag == QSEvac) {
    return cell->value;
  }
  return (Addr)0;
}

static void copy(Addr object, Addr to)
{
  QSCell cells, celld;

  cells = (QSCell)object;
  celld = (QSCell)to;

  *celld = *cells;
}
