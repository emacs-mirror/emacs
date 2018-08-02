/* 
TEST_HEADER
 id = $Id$
 summary = pointer to unaligned addr to fix (function)
 language = c
 link = testlib.o
END_HEADER
*/

#include <string.h>

#include "testlib.h"
#include "mpscamc.h"
#include "arg.h"

/* what follows is hacked from newfmt.c
*/

#include "mps.h"

/* tags */

enum {MCpadsingle, MCpadmany, MCheart, MCdata};

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 formatcomments   1   print comments on scanning, fixing, copying
 checkcomments    1   print comments on checking

*/

int formatcomments=0;
int checkcomments=0;
int countcomments=1;
int alloccomments=0;

int nextid=1000000;

enum
{
 SCANCALL_COUNT,
 SCANOBJ_COUNT, /* = #objects scanned (real ones, that is) */
 SCANPS_COUNT,  /* = #pad singles scanned */
 SCANPM_COUNT,  /* etc ... */
 SCANHEART_COUNT,
 COPY_COUNT,
 SKIP_COUNT,
 FWD_COUNT,
 ISFWD_COUNT,
 RESERVE_COUNT,
 ALLOC_COUNT,
 PAD_SINGLE_COUNT,
 PAD_MULTI_COUNT
};

int counters[PAD_MULTI_COUNT+1];
int prevcounters[PAD_MULTI_COUNT+1];
int maxcounters[PAD_MULTI_COUNT+1] = {0};

static long int maxcopy = 0;
static int freeze=0;

#define INCCOUNT(c) if(!freeze) counters[c]+=1

#define MAXSIZE 10000

/* a cell can be one of four things, depending on its type:
  MCpadsingle - a single pad item, MPS_PF_ALIGN in size
  MCpadmulti  - a longer pad item, at least 2*MPS_PF_ALIGN in size
  MCheart     - a broken heart, aka forwarding object
  MCdata      - a real object
*/

typedef int tag;

typedef union mycell mycell;

struct padsingle {tag tag;};

struct padmulti {tag tag; mps_addr_t next;};

struct heart {tag tag; size_t size; mps_addr_t obj;};

struct data
{
 tag tag;
 size_t size;
 long int id;
 long int copycount;
 long int numrefs;
 int checkedflag;
 struct refitem {mycell *addr; long int id;} ref[MAXSIZE];
};

union mycell
{
 tag tag;
 struct padsingle padsingle;
 struct padmulti  padmulti;
 struct heart     heart;
 struct data      data;
};

/* the scanning function doesn't try to fix null refs

   size is the total size of the object in bytes
   every other word from ref onwards, including the word
   at ref, is a reference; after each one comes the id
   of the object referenced, so we can check the graph is
   correct later!

nb: This way of doing things is not really guaranteed to work
   on all systems. It would be more correct to use an array
   of structures, or an array of unions.
*/

static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t myskip(mps_addr_t object);
static void myfwd(mps_addr_t object, mps_addr_t to);
static mps_addr_t myisfwd(mps_addr_t object);
static void mycopy(mps_addr_t object, mps_addr_t to);
static void mypad(mps_addr_t base, size_t size);

struct mps_fmt_A_s fmtA =
{
 MPS_PF_ALIGN,
 &myscan,
 &myskip,
 &mycopy,
 &myfwd,
 &myisfwd,
 &mypad
};

/* in the following, size is the number of refs you want
   the allocated object to have
*/

mycell *allocdumb(mps_ap_t ap, size_t size)
{
 mps_addr_t p;
 mycell *q;
 size_t bytes;
 size_t alignment;

 bytes = offsetof(struct data, ref) + size;

 alignment = MPS_PF_ALIGN; /* needed to make it as wide as size_t */

/* twiddle the value of size to make it aligned */
 bytes = (bytes+alignment-1) & ~(alignment-1);

 do
 {
  die(mps_reserve(&p, ap, bytes), "Reserve: ");
  INCCOUNT(RESERVE_COUNT);
  q=p;
  q->data.tag = MCdata;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.numrefs = 0;
  q->data.checkedflag = 0;
  q->data.size = bytes;
 }
 while (!mps_commit(ap, p, bytes));
 INCCOUNT(ALLOC_COUNT);
 commentif(alloccomments, "allocated id %li at %p.", nextid, q);
 nextid += 1;

 return q;
}

mycell *allocone(mps_ap_t ap, int size)
{
 mps_addr_t p;
 mycell *q;
 int i;
 size_t bytes;
 size_t alignment;

 bytes = offsetof(struct data, ref) + size*sizeof(struct refitem);

 alignment = MPS_PF_ALIGN; /* needed to make it as wide as size_t */

/* twiddle the value of size to make it aligned */
 bytes = (bytes+alignment-1) & ~(alignment-1);

 do
 {
  die(mps_reserve(&p, ap, bytes), "Reserve: ");
  INCCOUNT(RESERVE_COUNT);
  q=p;
  q->data.tag = MCdata;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.numrefs = size;
  q->data.checkedflag = 0;
  q->data.size = bytes;

  for(i=0; i<size; i+=1)
  {
   q->data.ref[i].addr = NULL;
   q->data.ref[i].id   = 0;
  }
 }
 while (!mps_commit(ap, p, bytes));
 INCCOUNT(ALLOC_COUNT);
 commentif(alloccomments, "allocated id %li at %p.", nextid, q);
 nextid += 1;

 return q;
}
 
static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 int i;

 INCCOUNT(SCANCALL_COUNT);
 MPS_SCAN_BEGIN(ss)
 {
  while (base < limit)
  {
   mycell *obj = base;
   mps_res_t res;
   mps_addr_t p;

   commentif(formatcomments, "scan %p.\n",  base);
   switch (obj->tag)
   {
    case MCpadsingle:
     INCCOUNT(SCANPS_COUNT);
     base = (mps_addr_t) ((char *) obj + sizeof(obj->padsingle));
     break;
    case MCpadmany:
     INCCOUNT(SCANPM_COUNT);
     base = (obj->padmulti.next);
     break;
    case MCdata:
     INCCOUNT(SCANOBJ_COUNT);
     /* actual scanning is done in here */

     for (i=0; i<(obj->data.numrefs); i++)
     {
      p = obj->data.ref[i].addr;
      if (p != NULL)
      {
       commentif(formatcomments, "fix: %d=%p.\n", i, p);

       /* copy ref to p for fixing, to avoid a pun (although
          the pun would probably work fine almost everywhere)
       */
       comment("About to fix with unaligned pointer...");
       p = UNALIGNED;
       res = MPS_FIX2(ss, &p);
       error("fix with unaligned pointer");
       if (res != MPS_RES_OK) return res;
       obj->data.ref[i].addr = p;
      }
     }
     base = (mps_addr_t) ((char *) obj + (obj->data.size));
     break;
    case MCheart:
     INCCOUNT(SCANHEART_COUNT);
     base = (mps_addr_t) ((char *) obj + (obj->heart.size));
     break;
    default:
     asserts(0, "scan: bizarre obj tag at %p.", obj);
   }
  }
 }
 MPS_SCAN_END(ss);
 return MPS_RES_OK;
}

static mps_addr_t myskip(mps_addr_t object)
{
 mycell *obj = object;

 INCCOUNT(SKIP_COUNT);
 switch(obj->tag)
 {
  case MCpadsingle:
   return (mps_addr_t) ((char *) obj + sizeof(obj->padsingle));
  case MCpadmany:
   return obj->padmulti.next;
  case MCheart:
   return (mps_addr_t) ((char *) obj + (obj->heart.size));
  case MCdata:
   return (mps_addr_t) ((char *) obj + (obj->data.size));
  default:
   asserts(0, "skip: bizarre obj tag at %p.", obj);
   return 0; /* not reached */
 }
}

static void mycopy(mps_addr_t object, mps_addr_t to)
{
 mycell *boj = object;
 mycell *toj = to;

 asserts(boj->tag = MCdata, "copy: non-data object");

 INCCOUNT(COPY_COUNT);
 commentif(formatcomments, "copy: %p -> %p\n", object, to);

/* this line would be bad, because the objects might overlap,
   and then C doesn't guarantee to do the right thing!

   *toj = *boj;
*/

 memmove(to, object, boj->data.size);
 if (!freeze)
 {
  toj->data.copycount = (toj->data.copycount)+1;
  if (toj->data.copycount > maxcopy) maxcopy = toj->data.copycount;
 }
}

/* pad stores not its size but a pointer to the next object,
   because we know we'll never be asked to copy it
*/

static void mypad(mps_addr_t base, size_t size)
{
 mycell *obj = base;

 asserts(size >= MPS_PF_ALIGN, "pad: size too small.");

 if (size == MPS_PF_ALIGN)
 {
  INCCOUNT(PAD_SINGLE_COUNT);
  obj->padsingle.tag = MCpadsingle;
 }
 else
 {
  asserts(size >= sizeof(struct padmulti), "pad: awkward size.");
  INCCOUNT(PAD_MULTI_COUNT);
  obj->padmulti.tag = MCpadmany;
  obj->padmulti.next = (mps_addr_t) ((char *) base + size);
 }
}

static mps_addr_t myisfwd(mps_addr_t object)
{
 mycell *obj = object;

 INCCOUNT(ISFWD_COUNT);
 if (obj->tag != MCheart)
 {
  return NULL;
 }
 else
 {
  return obj->heart.obj;
 }
}

static void myfwd(mps_addr_t object, mps_addr_t to)
{
 mycell *obj = object;
 size_t size;

 asserts(obj->tag == MCdata || obj->tag == MCheart,
  "fwd: unexpected object tag at %p.", obj);
 INCCOUNT(FWD_COUNT);

 if (obj->tag == MCdata)
 {
  size = obj->data.size;
 }
 else /* obj->tag == MCheart */
 {
  size = obj->heart.size;
 }
 obj->data.tag = MCheart;
 obj->heart.obj = to;
 obj->heart.size = size;
}


/* ---------------------------------------------------------------
   Access methods for mycell objects
*/

/* set the nth reference of obj to to (n from 0 to size-1) */

void setref(mycell *obj, int n, mycell *to)
{
 asserts(obj->tag = MCdata, "setref: from non-data object.");
 asserts(to->tag = MCdata, "setref: to non-data object.");
 asserts(obj->data.numrefs > n, "setref: access beyond object size.");

 obj->data.ref[n].addr = to;
 obj->data.ref[n].id = to->data.id;
}

mycell *getref(mycell *obj, int n)
{
 asserts(obj->tag = MCdata, "getref: from non-data object.");
 asserts(obj->data.numrefs > n, "getref: access beyond object size.");
 return obj->data.ref[n].addr;
}

mps_addr_t getdata(mycell *obj)
{
 return (mps_addr_t) &(obj->data.ref[0]);
}

long int getid(mycell *obj)
{
 asserts(obj->tag = MCdata, "getid: non-data object.");
 return obj->data.id;
}

long int getcopycount(mycell *obj)
{
 asserts(obj->tag = MCdata, "getcopycount: non-data object.");
 return obj->data.copycount;
}

long int getsize(mycell *obj)
{
 asserts(obj->tag = MCdata, "getsize: non-data object.");
 return obj->data.numrefs;
}

/* ---- Now the test itself! ---- */

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t ap;

 int j;
 mycell *a, *b;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 formatcomments = 0;

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 a = allocone(ap, 100);
 setref(a, 0, a);

 for (j=1; j<1000; j++)
 {
  b = allocone(ap, 1000);
  setref(b, 0, a);
  a = b;
 }

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}
