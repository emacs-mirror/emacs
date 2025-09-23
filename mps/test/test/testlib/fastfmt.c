/* $Id$
fastfmt.c
   See comments in header file for usage.
*/

#include "fastfmt.h"
#include <string.h>

long int nextid=0x1000000;
long int checkobjcount=0;

mps_addr_t exfmt_root=NULL;

struct wrapper {
 struct wrapper *ww;
 mps_word_t tag;
 mps_word_t fixedlen;
 mps_word_t newfixedlen;
};

struct wrapper wrapobj = {
 &wrapobj,
 0x36ABBE6,
 0x5,
 0x5
};

mycell *wrapper = (mycell *) &wrapobj;

/* a cell can be one of four things, depending on its type:
  MCpad       - a pad item
  MCheart     - a broken heart, aka forwarding object
  MCdata      - a real object
*/

/* the scanning function doesn't try to fix null refs
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

mycell *allocdumb(mps_ap_t ap, size_t size, mps_rank_t rank)
{
 mycell *q;
 size_t bytes;
 size_t alignment;

 bytes = offsetof(struct data, ref) + size;

 alignment = MPS_PF_ALIGN; /* needed to make it as wide as size_t */

/* twiddle the value of size to make it aligned */
 bytes = (bytes+alignment-1) & ~(alignment-1);

 do
 {
  die(mps_reserve(&exfmt_root, ap, bytes), "Reserve: ");
  q=exfmt_root;
  q->data.tag = (mps_word_t) wrapper;
  q->data.assoc = NULL;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.numrefs = 0;
  q->data.checkedflag = 0;
  q->data.rank = rank;
  q->data.size = bytes;
 }
 while (!mps_commit(ap, exfmt_root, bytes));
 nextid += 1;

 return q;
}

mycell *allocone(mps_ap_t ap, int size, mps_rank_t rank)
{
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
  die(mps_reserve(&exfmt_root, ap, bytes), "Reserve: ");
  q=exfmt_root;
  q->data.tag = MCdata + (mps_word_t) wrapper;
  q->data.assoc = NULL;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.numrefs = size;
  q->data.checkedflag = 0;
  q->data.rank = rank;
  q->data.size = bytes;

  for(i=0; i<size; i+=1)
  {
   q->data.ref[i].addr = NULL;
   q->data.ref[i].id   = 0;
  }
 }
 while (!mps_commit(ap, exfmt_root, bytes));
 nextid += 1;

 return q;
}
 
static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 int i;

 MPS_SCAN_BEGIN(ss)
 {
  while (base < limit)
  {
   mycell *obj = base;
   mps_res_t res;
   mps_addr_t p;

   switch (obj->tag & 0x3)
   {
    case MCpad:
     base = (mps_addr_t) (obj->pad.tag &~ (mps_word_t) 3);
     break;
    case MCdata:
     /* actual scanning is done in here */
     /* make sure to fix the assoc pointer first */
     p = obj->data.assoc;
     if (p != NULL) {
      res = MPS_FIX12(ss, (mps_addr_t *) &p);
      if (res != MPS_RES_OK) return res;
      obj->data.assoc = p;
     }

     for (i=0; i<(obj->data.numrefs); i++)
     {
      p = obj->data.ref[i].addr;
      if (p != NULL)
      {
       res = MPS_FIX12(ss, (mps_addr_t *) &p);
       if (res != MPS_RES_OK) return res;
       obj->data.ref[i].addr = p;
      }
     }
     base = (mps_addr_t) ((char *) obj + (obj->data.size));
     break;
    case MCheart:
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

 switch(obj->tag & 0x3)
 {
  case MCpad:
   return (mps_addr_t) (obj->pad.tag &~ (mps_word_t) 0x3);
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

 memmove(to, object, boj->data.size);
}

/* pad stores not its size but a pointer to the next object,
   because we know we'll never be asked to copy it
*/

static void mypad(mps_addr_t base, size_t size)
{
 mycell *obj = base;

 obj->pad.tag = MCpad + (mps_word_t) ((char *) base + size);
}

static mps_addr_t myisfwd(mps_addr_t object)
{
 mycell *obj = object;

 if ((obj->tag & 3) != MCheart)
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

 if ((obj->tag & 3) == MCdata)
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
 asserts(obj->tag == MCdata + (mps_word_t) wrapper,
  "setref: from non-data object at %p", obj);
 asserts((to==NULL)||((to->tag & 3) == MCdata),
  "setref: to non-data object at %p", to);
 asserts(obj->data.numrefs > n, "setref: access beyond object size.");

 obj->data.ref[n].addr = to;
 obj->data.ref[n].id = (to==NULL ? 0 : to->data.id);
}

mycell *getref(mycell *obj, int n)
{
 asserts(obj->tag == MCdata + (mps_word_t) wrapper,
  "getref: from non-data object.");
 asserts(obj->data.numrefs > n, "getref: access beyond object size.");
 return obj->data.ref[n].addr;
}

mps_addr_t getdata(mycell *obj)
{
 return (mps_addr_t) &(obj->data.ref[0]);
}

mps_addr_t getassociated(mps_addr_t addr)
{
  mycell *obj = addr;
  return (mps_addr_t) &(obj->data.ref[1]);
}

long int getid(mycell *obj)
{
 asserts(obj->tag == MCdata + (mps_word_t) wrapper,
  "getid: non-data object.");
 return obj->data.id;
}

long int getsize(mycell *obj)
{
 asserts(obj->tag == MCdata + (mps_word_t) wrapper,
  "getsize: non-data object.");
 return obj->data.numrefs;
}

/* ---------------------------------------------------------------
   Now the useful things specially for checking the graph
*/

/* recursively check the graph, starting at an object.
   We do the check twice, so as to restore the
   checkflags to zero.
*/

static void checkloop(mycell *obj, int dir)
{
 mycell *toj;
 int tid;
 int i;

 asserts(obj->tag == MCdata + (mps_word_t) wrapper,
  "checkfrom: non data object in graph at %p.", obj);

 if (obj->data.checkedflag != dir)
 {
  checkobjcount += 1;

  obj->data.checkedflag = dir;

  if (obj->data.assoc != NULL) {
   toj = obj->data.assoc;
   checkloop(toj, dir);
  }
  for (i=0; i<(obj->data.numrefs); i+=1)
  {
   if (obj->data.ref[i].addr != NULL)
   {
    toj = (obj->data.ref[i].addr);
    tid = (obj->data.ref[i].id);
    asserts(toj->data.id == tid,
     "checkfrom: corrupt graph at %p, %d.", obj, i);
    checkloop(toj, dir);
   }
  }
 }
}

void checkfrom(mycell *obj)
{
 checkobjcount = 0;
 checkloop(obj, 1);
 comment("checkfrom: %li live objects checked", checkobjcount);
 checkloop(obj, 0);
 comment("checkfrom: graph ok from ID: %li.", obj->data.id);
}

