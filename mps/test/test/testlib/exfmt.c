/* $Id$
exfmt.c
   A format for the awl and amc pools which can cope with exact references
   See comments in header file for usage.
*/

#include "exfmt.h"
#include <string.h>

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

*/

int formatcomments=0;
int checkcomments=0;
int countcomments=1;
int alloccomments=0;
int fixcomments=0;
int deathcomments=1;
int skipcomments=0;
int splurgeassoc=0; /* write madly to associated objects */

long int nextid=0x1000000;

long int checkobjcount=0;

mps_addr_t exfmt_root=NULL;

int counters[PAD_COUNT+1] = {0};
int prevcounters[PAD_COUNT+1] = {0};
int maxcounters[PAD_COUNT+1] = {0};

long int maxcopy = 0;
int freeze=0;

/* The AWL pool makes certain assumptions about the object format,
   some necessary and some unnecessary but useful for detecting problems
   when using fmtdy. We have to ensure that:

   Each objects begins with a pointer to a wrapper. To tag objects,
   use non-zero values for the low bits.

   The second word in each object is a pointer to the associated
   object, usual rules for references apply. The getassociated()
   function looks up this reference and can be used as the
   find_dependent function when creating an AWL pool.

   The wrapper begins with a wrapper-wrapper pointer, and the third word
   of the wrapper object has bit 0 set and bit 1 clear and at least one higher
   bit also set. The fourth word is the same, to cater for the new Dylan
   object format used by honeybee.3 and later releases.

   The wrapper-wrapper's first word points to itself.

   Here I make the wrapper-wrapper equal to the wrapper for all objects,
   and use the same wrapper for all objects.

   The other pools don't care about the wrapper, of course.
*/

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

#define INCCOUNT(c) do {if(!freeze) counters[c]+=1;} while (0)
#define INCCOUNTIF(f, c) do {if(f) INCCOUNT(c);} while (0)

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

mycell *allocdumb(mps_ap_t ap, size_t size, int countflag)
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
  INCCOUNTIF(countflag, RESERVE_COUNT);
  q=exfmt_root;
  q->data.tag = (mps_word_t) wrapper;
  q->data.assoc = NULL;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.numrefs = 0;
  q->data.checkedflag = 0;
  q->data.countflag = countflag;
  q->data.size = bytes;
 }
 while (!mps_commit(ap, exfmt_root, bytes));
 INCCOUNTIF(countflag, ALLOC_COUNT);
 commentif(alloccomments, "allocated id %li at %p.", nextid, q);
 nextid += 1;

 return q;
}

mycell *allocone(mps_ap_t ap, int size, int countflag)
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
  INCCOUNTIF(countflag, RESERVE_COUNT);
  q=exfmt_root;
  q->data.tag = MCdata + (mps_word_t) wrapper;
  q->data.assoc = NULL;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.numrefs = size;
  q->data.checkedflag = 0;
  q->data.countflag = countflag;
  q->data.size = bytes;

  for(i=0; i<size; i+=1)
  {
   q->data.ref[i].addr = NULL;
   q->data.ref[i].id   = 0;
  }
 }
 while (!mps_commit(ap, exfmt_root, bytes));
 INCCOUNTIF(countflag, ALLOC_COUNT);
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

   switch (obj->tag & 0x3)
   {
    case MCpad:
     INCCOUNT(SCANPAD_COUNT);
     base = (mps_addr_t) (obj->pad.tag &~ (mps_word_t) 3);
     break;
    case MCdata:
     /* actual scanning is done in here */
     asserts(obj->tag == MCdata + (mps_word_t) wrapper, "scan on bad object");
     asserts(obj->data.id != MCerrorid, "scan on error object");
     INCCOUNTIF(obj->data.countflag, SCANOBJ_COUNT);
     commentif(formatcomments, "scan %li at %p.", obj->data.id, base);

     /* make sure to fix the assoc pointer first */
     p = obj->data.assoc;
     if (p != NULL) {
      commentif(fixcomments, "fix %li[assoc]", obj->data.id);
      res = MPS_FIX(ss, (mps_addr_t *) &p);
      if (res != MPS_RES_OK) return res;
      if (p == NULL) {
       commentif(deathcomments, "fixed %li[assoc] to NULL", obj->data.id);
       INCCOUNTIF(obj->data.countflag, DYING_REFERENCE_COUNT);
      }
      obj->data.assoc = p;
     }

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
       if (p == NULL) {
        commentif(deathcomments, "fixed %li[%i] to NULL", obj->data.id, i);
        INCCOUNTIF(obj->data.countflag, DYING_REFERENCE_COUNT);
       }
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

 commentif(skipcomments, "skip %p.", object);
 INCCOUNT(SKIP_COUNT);
 switch(obj->tag & 0x3)
 {
  case MCpad:
   return (mps_addr_t) (obj->pad.tag &~ (mps_word_t) 0x3);
  case MCheart:
   return (mps_addr_t) ((char *) obj + (obj->heart.size));
  case MCdata:
   asserts(obj->tag == MCdata + (mps_word_t) wrapper,
    "skip on bad obj at %p.", obj);
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

 asserts(boj->tag == MCdata + (mps_word_t) wrapper,
  "copy: non-data object");

 INCCOUNTIF(boj->data.countflag, COPY_COUNT);
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

 INCCOUNT(PAD_COUNT);
 obj->pad.tag = MCpad + (mps_word_t) ((char *) base + size);
}

static mps_addr_t myisfwd(mps_addr_t object)
{
 mycell *obj = object;

 INCCOUNT(ISFWD_COUNT);
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

 asserts(((obj->tag & 3) == MCdata) || ((obj->tag & 3) == MCheart),
  "fwd: unexpected object tag at %p.", obj);
 INCCOUNT(FWD_COUNT);

 if ((obj->tag & 3) == MCdata)
 {
  asserts(obj->tag == MCdata + (mps_word_t) wrapper,
   "fwd: bad obj at %p.", obj);
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

long int getcopycount(mycell *obj)
{
 asserts(obj->tag == MCdata + (mps_word_t) wrapper,
  "getcopycount: non-data object.");
 return obj->data.copycount;
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
  commentif(checkcomments, "checking %p = %li", obj, obj->data.id);

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
 for (i=0; i < PAD_COUNT+1; i++)
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
 for (i=0; i < PAD_COUNT+1; i++)
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
 d_c(SCANPAD_COUNT, "pad scans");
 d_c(COPY_COUNT, "copys");
 d_c(SKIP_COUNT, "skips");
 d_c(FWD_COUNT, "fwds");
 d_c(ISFWD_COUNT, "isfwds");
 d_c(PAD_COUNT, "pads");
 d_c(RESERVE_COUNT, "reserve calls");
 d_c(ALLOC_COUNT, "allocations");
 d_c(DYING_REFERENCE_COUNT, "references fixed to NULL");
 comment("--------");
}

void displaymaxcounters(void)
{
 comment("--------");
 comment("Maximum counter values:");
 d_mc(SCANCALL_COUNT, "scan calls");
 d_mc(SCANOBJ_COUNT, "object scans");
 d_mc(SCANHEART_COUNT, "heart scans");
 d_mc(SCANPAD_COUNT, "pad scans");
 d_mc(COPY_COUNT, "copys");
 d_mc(SKIP_COUNT, "skips");
 d_mc(FWD_COUNT, "fwds");
 d_mc(ISFWD_COUNT, "isfwds");
 d_mc(RESERVE_COUNT, "reserve calls");
 d_mc(ALLOC_COUNT, "allocations");
 d_mc(DYING_REFERENCE_COUNT, "references fixed to NULL");
 d_mc(PAD_COUNT, "pads");
 comment("--------");
 comment("max copies of a single object: %li.", maxcopy);
 comment("--------");
}

