/* $Id$
lofmt.c
   A format for pool class LO
*/

#include "lofmt.h"
#include <string.h>

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 alloclocomments  comment on allocation (0)
 allowlocopies    allow objects to be copied (1)
*/

int alloclocomments=0;
int allowlocopies=1;

long int nextid=0x2000000;

/* a cell can be one of four things, depending on its type:
  LOpadsingle - a single pad item, MPS_PF_ALIGN in size
  LOpadmulti  - a longer pad item, at least 2*MPS_PF_ALIGN in size
  LOheart     - a broken heart, aka forwarding object
  LOdata      - a real object
*/

static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t myskip(mps_addr_t object);
static void myfwd(mps_addr_t object, mps_addr_t to);
static mps_addr_t myisfwd(mps_addr_t object);
static void mycopy(mps_addr_t object, mps_addr_t to);
static void mypad(mps_addr_t base, size_t size);

struct mps_fmt_A_s fmtLO =
{
 MPS_PF_ALIGN,
 &myscan,
 &myskip,
 &mycopy,
 &myfwd,
 &myisfwd,
 &mypad
};

/* in the following, size is the size_t of the data element
   of the lodata structure you want the object to have; i.e.
   the number of bytes of storage you get in the object on top
   of the headers.
*/

locell *alloclo(mps_ap_t ap, size_t size) {
 mps_addr_t p;
 locell *q;
 size_t bytes;
 size_t alignment;

 bytes = offsetof(struct lodata, data) + size;

 alignment = MPS_PF_ALIGN; /* needed to make it as wide as size_t */

/* twiddle the value of size to make it aligned */
 bytes = (bytes+alignment-1) & ~(alignment-1);

 do {
  die(mps_reserve(&p, ap, bytes), "Reserve: ");
  q=p;
  q->data.tag = LOdata;
  q->data.id = nextid;
  q->data.copycount = 0;
  q->data.size = bytes;
 }
 while (!mps_commit(ap, p, bytes));
 commentif(alloclocomments, "allocated id %li at %p.", nextid, q);
 nextid += 1;

 return q;
}

static mps_res_t myscan(mps_ss_t ss, mps_addr_t b, mps_addr_t l) {
 error("Scan method in LO format called!");
 return MPS_RES_OK;
}

static mps_addr_t myskip(mps_addr_t object) {
 locell *obj = object;

 switch(obj->tag)
 {
  case LOpadsingle:
   return (mps_addr_t) ((char *) obj + MPS_PF_ALIGN);
  case LOpadmany:
   return obj->padmulti.next;
  case LOheart:
   return (mps_addr_t) ((char *) obj + (obj->heart.size));
  case LOdata:
   return (mps_addr_t) ((char *) obj + (obj->data.size));
  default:
   asserts(0, "loskip: bizarre obj tag at %p.", obj);
   return 0; /* not reached */
 }
}

static void mycopy(mps_addr_t object, mps_addr_t to)
{
 locell *boj = object;
 locell *toj = to;

 asserts(allowlocopies, "locopy on LO object");
 asserts(boj->tag == LOdata, "locopy: non-data object");

 memmove(to, object, boj->data.size);
 toj->data.copycount = (toj->data.copycount)+1;
}

/* pad stores not its size but a pointer to the next object,
   because we know we'll never be asked to copy it
*/

static void mypad(mps_addr_t base, size_t size)
{
 locell *obj = base;

 asserts(size >= MPS_PF_ALIGN, "pad: size too small.");

 if (size == MPS_PF_ALIGN)
 {
  asserts(sizeof(obj->padsingle) <= MPS_PF_ALIGN, "impossible pad");
  obj->padsingle.tag = LOpadsingle;
 }
 else
 {
  asserts(size >= sizeof(struct lopadmulti), "pad: awkward size.");
  obj->padmulti.tag = LOpadmany;
  obj->padmulti.next = (mps_addr_t) ((char *) base + size);
 }
}

static mps_addr_t myisfwd(mps_addr_t object)
{
 locell *obj = object;

 if (obj->tag != LOheart)
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
 locell *obj = object;
 size_t size;

 asserts(obj->tag == LOdata || obj->tag == LOheart,
  "lofwd: unexpected object tag at %p.", obj);

 if (obj->tag == LOdata)
 {
  size = obj->data.size;
 }
 else /* obj->tag == LOheart */
 {
  size = obj->heart.size;
 }
 obj->data.tag = LOheart;
 obj->heart.obj = to;
 obj->heart.size = size;
}

long int getloid(locell *obj)
{
 asserts(obj->tag == LOdata, "getloid: non-data object.");
 return obj->data.id;
}

long int getlocopycount(locell *obj)
{
 asserts(obj->tag == LOdata, "getlocopycount: non-data object.");
 return obj->data.copycount;
}

long int getlosize(locell *obj)
{
 asserts(obj->tag == LOdata, "getlosize: non-data object.");
 return obj->data.size - offsetof(struct lodata, data);
}

