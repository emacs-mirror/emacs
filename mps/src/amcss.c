/*  ==== POOL CLASS AMC TEST ====
 *
 *  $HopeName: MMsrc!amcss.c(trunk.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a unit stress test for the AMC pool class.  It uses a simple
 *  object format to do a few collections.  It's a bit of a hack at the
 *  moment.
 */

#include "std.h"
#include "mps.h"
#include "lib.h"
#include "deque.h"
#include "space.h"
#include "root.h"
#include "trace.h"
#include "format.h"
#include "pool.h"
#include "amc.h"
#include "buffer.h"
#include "coll.h"
#include "prot.h"
#include "testlib.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>


#define NR_EXACT_ROOTS	50
#define NR_AMBIG_ROOTS	50
#define FIELDS_MAX	2000
#define OBJECTS		4000
#define OBJECT_ALIGN	sizeof(mps_addr_t *)


static mps_pool_t pool;
static mps_ap_t buffer;
static mps_addr_t exactRoots[NR_EXACT_ROOTS];
static mps_addr_t ambigRoots[NR_AMBIG_ROOTS];
static mps_word_t name = 0xA << (ADDRWIDTH-4);


#define OBJHERE(object)		(((object)[0] & 1uL) == 1uL)
#define OBJHEAD(object)		((object)[0])
#define OBJFIELDS(object)	((object)[0] >> 1)
#define OBJFIELD(object, n)	((object)[2+(n)])
#define OBJNAME(object)		((object)[1])
#define OBJSUM(object)		checksum(object, OBJFIELDS(object)+2)


static unsigned long invrnd(unsigned long n)
{
  return n/((rnd()%n)+1)-1;
}


static mps_word_t checksum(mps_word_t *base, mps_word_t size)
{
  mps_word_t sum = 0xBA5E5EED;
  
  while(size--)
    sum += (mps_word_t)*base++;
  
  return sum;
}


static mps_bool_t probe(mps_addr_t where)
{
  mps_word_t *object;
  mps_word_t fields, i;

  if(PoolHasAddr((Pool)pool, (Addr)where) &&
     IsAligned(OBJECT_ALIGN, (Addr)where))
  {
    object = (mps_word_t *)where;
    if(OBJHERE(object))
    {
      fields = OBJFIELDS(object);
      if(fields <= FIELDS_MAX &&
         PoolHasAddr((Pool)pool, (Addr)&OBJFIELD(object, fields)))
      {
	for(i=0; i<fields; ++i)
	  if(OBJFIELD(object, i) != 0xDECEA5ED)
	    if(!PoolHasAddr((Pool)pool, OBJFIELD(object, i)))
	      return FALSE;

	if(OBJFIELD(object, i) == OBJSUM(object))
          return TRUE;
      }
    }
  }
  
  return FALSE;
}


static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  Error e;
  mps_word_t *object;
  mps_word_t i;
  mps_word_t fields;
  
  AVER(base < limit);

  while(base < limit)
  {
    AVER(probe(base));

    object = (Addr *)base;

    fields = OBJFIELDS(object);

    for(i=0; i<fields; ++i) {
      e = mps_fix(ss, (mps_addr_t)&OBJFIELD(object, i));
      if(e != ErrSUCCESS) return e;
    }

    OBJFIELD(object, i) = OBJSUM(object);

    base = (mps_addr_t)(&OBJFIELD(object, fields) + 1);
  }
  return ErrSUCCESS;
}

static mps_addr_t skip(mps_addr_t where)
{
  mps_word_t *object;

  AVER(probe(where));
  object = (mps_word_t *)where;
  return (&OBJFIELD(object, OBJFIELDS(object)) + 1);
}

static mps_word_t length(mps_addr_t where)
{
  return (mps_word_t)skip(where) - (mps_word_t)where;
}

/* A broken heart is the new address of the object, which */
/* has the LSB clear. */

static mps_addr_t isfwd(mps_addr_t where)
{
  mps_word_t *object;
  
  AVER(PoolHasAddr((Pool)pool, (Addr)where));
  AVER(IsAligned(OBJECT_ALIGN, (Addr)where));

  object = (Addr *)where;

  if(OBJHERE(object)) {
    return NULL;
  } else {
    return (mps_addr_t)object[0];
  }
}

static void copy(mps_addr_t from, mps_addr_t to)
{
  mps_word_t l;

  AVER(IsAligned(OBJECT_ALIGN, (Addr)to));
  AVER(probe(from));

  l = length(from);
  memcpy((void *)to, (void *)from, l);
  AVER(probe(to));
}

static void fwd(mps_addr_t from, mps_addr_t to)
{
  mps_word_t l;

  AVER(IsAligned(OBJECT_ALIGN, (Addr)to));
  AVER(probe(from));
  AVER(probe(to));

  l = length(from);
  memset((void *)from, (char)0xEE, (size_t)l);
  *(mps_addr_t *)from = to;
}


/* make an object with random length.  The header is the length */
/* shifted left one with the LSB set. */

static mps_addr_t make(void)
{
  mps_addr_t new;
  mps_word_t *object;
  mps_word_t fields = invrnd(FIELDS_MAX);
  mps_word_t i;
  mps_word_t size = sizeof(mps_addr_t) * (fields + 3);
  
  do {
    die(mps_reserve(&new, buffer, size), "reserve");
    object = (mps_word_t *)new;
    OBJHEAD(object) = (fields << 1) | 1;
    OBJNAME(object) = (mps_word_t)name++;
    for(i=0; i<fields; ++i)
      OBJFIELD(object, i) = (mps_word_t)exactRoots[rnd() % NR_EXACT_ROOTS];
    OBJFIELD(object, i) = OBJSUM(object);
    AVER(probe(new));
    AVER(length(new) == size);
  } while(!mps_commit(buffer, new, size));

  return new;
}


static void *test(void *arg, size_t s)
{
  mps_space_t space;
  mps_form_t format;
  mps_root_t exactRoot, ambigRoot;
  mps_word_t i, j;
  unsigned collections;
  Coll coll;
  mps_form_A_s f;

  space = (mps_space_t)arg;
  UNUSED(s);

  f.align = OBJECT_ALIGN;
  f.scan = scan;
  f.skip = skip;
  f.fwd = fwd;
  f.isfwd = isfwd;
  f.copy = copy;

  die(mps_form_create_A(&format, space, &f), "FormatCreate");

  die(mps_pool_create(&pool, space, (mps_class_t)PoolClassAMC(),
                 format), "PoolCreate");
  die(mps_ap_create(&buffer, pool), "BufferCreate");

  die(mps_root_create_table(&exactRoot, space,
  		      MPS_RANK_EXACT, (mps_rm_t)0,
  		      &exactRoots[0], NR_EXACT_ROOTS),
  		      "RootCreateTable");

  die(mps_root_create_table(&ambigRoot, space,
  		      RefRankAMBIG, (mps_rm_t)0,
  		      &ambigRoots[0], NR_AMBIG_ROOTS),
  		      "RootCreateTable");

  for(i=0; i<NR_EXACT_ROOTS; ++i)
    exactRoots[i] = (mps_addr_t)0xDECEA5ED;

  for(i=0; i<NR_AMBIG_ROOTS; ++i)
    ambigRoots[i] = (mps_addr_t)rnd();
    
  collections = 0;

  for(i=0; i<OBJECTS; ++i)
  {
    unsigned c;

    if(i == 2000) {
      die(CollCreate(&coll, (Pool)pool), "CollCreate");
      die(SchedProcAdd(SpaceSched((Space)space),
	  CollProc, coll, 0), "SchedProcAdd");
      {int k; for(k=0; k<NR_EXACT_ROOTS; ++k) probe(exactRoots[k]);}
    }

    c = AMCCollections((Pool)pool);

    if(collections != c)
    {
      collections = c;

      printf("\nCollection %u, %lu objects.\n",
             c, (unsigned long)i);

/*      PoolDescribe(pool, LibStreamOut()); */
      SpaceDescribe((Space)space, LibStreamOut());

      for(j=0; j<NR_EXACT_ROOTS; ++j)
        if((mps_word_t)exactRoots[j] != 0xDECEA5ED)
          AVER(probe(exactRoots[j]));
    }

    if(rnd() & 1) {
      exactRoots[invrnd(NR_EXACT_ROOTS)] = make();
      probe(exactRoots[invrnd(NR_EXACT_ROOTS)]);
    } else {
      ambigRoots[invrnd(NR_AMBIG_ROOTS)] = make();
      probe(ambigRoots[invrnd(NR_AMBIG_ROOTS)]);
    }

    /* roots[invrnd(NR_ROOTS)] = 0xDECEA5ED; */
  }
  
  mps_ap_destroy(buffer);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(pool);

  return NULL;
}

int main(void)
{
  mps_space_t space;
/*  mps_thr_t thread; */
  void *r;
  
  die(mps_space_create(&space), "SpaceCreate");

/*  die(mps_thread_reg(&thread, space), "ThreadReg"); */

  mps_tramp(&r, test, space, 0);

/*  mps_thread_dereg(thread); */

  mps_space_destroy(space);
  
  return 0;
}
