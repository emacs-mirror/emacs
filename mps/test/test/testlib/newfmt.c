/* $Id$
newfmt.c
   My attempt to write a format using unions &c to avoid
   nasty casting all over the place
*/

#include "newfmt.h"
#include <string.h>

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
int fixcomments=0;

long int nextid=0x1000000;

long int checkobjcount=0;

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

#define INCCOUNT(c) do {if(!freeze) counters[c]+=1;} while (0)

/* a cell can be one of four things, depending on its type:
  MCpadsingle - a single pad item, MPS_PF_ALIGN in size
  MCpadmulti  - a longer pad item, at least 2*MPS_PF_ALIGN in size
  MCheart     - a broken heart, aka forwarding object
  MCdata      - a real object
*/

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

   switch (obj->tag)
   {
    case MCpadsingle:
     INCCOUNT(SCANPS_COUNT);
     base = (mps_addr_t) ((char *) obj + MPS_PF_ALIGN);
     break;
    case MCpadmany:
     INCCOUNT(SCANPM_COUNT);
     base = (obj->padmulti.next);
     break;
    case MCdata:
     INCCOUNT(SCANOBJ_COUNT);
     /* actual scanning is done in here */

     asserts(obj->data.id != MCerrorid, "scan on error object");
     commentif(formatcomments, "scan %li at %p.", obj->data.id, base);
     for (i=0; i<(obj->data.numrefs); i++)
     {
      p = obj->data.ref[i].addr;
      if (p != NULL)
      {
       /* copy ref to p for fixing, to avoid a pun (although
          the pun would probably work fine almost everywhere)
       */
       commentif(fixcomments, "fix %li[%i] -> %li",
        obj->data.id, i, obj->data.ref[i].id);
       res = MPS_FIX(ss, (mps_addr_t *) &p);
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
   return (mps_addr_t) ((char *) obj + MPS_PF_ALIGN);
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

 asserts(boj->tag == MCdata, "copy: non-data object");

 INCCOUNT(COPY_COUNT);
 commentif(formatcomments, "copy: %li: %p -> %p\n",
  boj->data.id, object, to);

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
  asserts(sizeof(obj->padsingle) <= MPS_PF_ALIGN, "impossible pad");
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
 asserts(obj->tag == MCdata, "setref: from non-data object.");
 asserts((to==NULL)||(to->tag == MCdata), "setref: to non-data object.");
 asserts(obj->data.numrefs > n, "setref: access beyond object size.");

 obj->data.ref[n].addr = to;
 obj->data.ref[n].id = (to==NULL ? 0 : to->data.id);
}

mycell *getref(mycell *obj, int n)
{
 asserts(obj->tag == MCdata, "getref: from non-data object.");
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
 asserts(obj->tag == MCdata, "getid: non-data object.");
 return obj->data.id;
}

long int getcopycount(mycell *obj)
{
 asserts(obj->tag == MCdata, "getcopycount: non-data object.");
 return obj->data.copycount;
}

long int getsize(mycell *obj)
{
 asserts(obj->tag == MCdata, "getsize: non-data object.");
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

 asserts(obj->tag == MCdata,
  "checkfrom: non data object in graph at %p.", obj);

 if (obj->data.checkedflag != dir)
 {
  commentif(checkcomments, "checking %p = %li", obj, obj->data.id);

  checkobjcount += 1;

  obj->data.checkedflag = dir;

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
 int k;

 freeze = 1; /* suspend counting while checking graph */
 checkobjcount = 0;
 checkloop(obj, 1);
 comment("checkfrom: %li live objects checked", checkobjcount);
 k = checkcomments;
 checkcomments = 0;
 checkloop(obj, 0);
 checkcomments = k;
 comment("checkfrom: graph ok from ID: %li.", obj->data.id);
 freeze = 0; /* resume counting */
}

/* ----------------------------------------------------------
   Now things to reset and display the counters
*/

void resetcounters(void)
{
 int i;
 for (i=0; i < PAD_MULTI_COUNT+1; i++)
 {
  counters[i]=0;
  prevcounters[i]=0;
  maxcounters[i]=0;
 }
 maxcopy = 0;
}

void updatecounters(void)
{
 int i;
 for (i=0; i < PAD_MULTI_COUNT+1; i++)
 {
  if (counters[i]-prevcounters[i] > maxcounters[i])
  {
   maxcounters[i]=counters[i]-prevcounters[i];
  }
  prevcounters[i]=counters[i];
 }
}


static void d_c(int i, char *name)
{
 comment("%10d %s", counters[i], name);
}

static void d_mc(int i, char *name)
{
 comment("%10d %s", maxcounters[i], name);
}

void displaycounters(void)
{
 comment("--------");
 comment("Counters:");
 d_c(SCANCALL_COUNT, "scan calls");
 d_c(SCANOBJ_COUNT, "object scans");
 d_c(SCANHEART_COUNT, "heart scans");
 d_c(SCANPS_COUNT, "pad scans (single word)");
 d_c(SCANPM_COUNT, "pad scans (multi word)");
 d_c(COPY_COUNT, "copys");
 d_c(SKIP_COUNT, "skips");
 d_c(FWD_COUNT, "fwds");
 d_c(ISFWD_COUNT, "isfwds");
 d_c(PAD_SINGLE_COUNT, "pads (single word)");
 d_c(PAD_MULTI_COUNT, "pads (multi word)");
 d_c(RESERVE_COUNT, "reserve calls");
 d_c(ALLOC_COUNT, "allocations");
 comment("--------");
}

void displaymaxcounters(void)
{
 comment("--------");
 comment("Maximum counter values:");
 d_mc(SCANCALL_COUNT, "scan calls");
 d_mc(SCANOBJ_COUNT, "object scans");
 d_mc(SCANHEART_COUNT, "heart scans");
 d_mc(SCANPS_COUNT, "pad scans (single word)");
 d_mc(SCANPM_COUNT, "pad scans (multi word)");
 d_mc(COPY_COUNT, "copys");
 d_mc(SKIP_COUNT, "skips");
 d_mc(FWD_COUNT, "fwds");
 d_mc(ISFWD_COUNT, "isfwds");
 d_mc(RESERVE_COUNT, "reserve calls");
 d_mc(ALLOC_COUNT, "allocations");
 d_mc(PAD_SINGLE_COUNT, "pads (single word)");
 d_mc(PAD_MULTI_COUNT, "pads (multi word)");
 comment("--------");
 comment("max copies of a single object: %li.", maxcopy);
 comment("--------");
}

