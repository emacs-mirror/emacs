/* $HopeName: MMQA_harness!testlib:epvmfmt.c(trunk.6) $
*/

#include "epvmfmt.h"
#include <string.h>

int alloccomments = 0;

/* the scanning function doesn't try to fix null refs
*/

static mps_res_t epvmscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t epvmskip(mps_addr_t object);
static void epvmfwd(mps_addr_t object, mps_addr_t to);
static mps_addr_t epvmisfwd(mps_addr_t object);
static void epvmcopy(mps_addr_t object, mps_addr_t to);
static void epvmpad(mps_addr_t base, size_t size);

static mps_res_t epvmscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);

mps_addr_t epvmskip(mps_addr_t object) {
/* error("skip called on EPVM object: %p", object);
*/
 asserts(((unsigned long) object & 7) == 0,
  "epvmskip called on unaligned object %p", object);
 return (mps_addr_t) ((char *)object + 8);
}

void epvmfwd(mps_addr_t object, mps_addr_t to) {
 error("fwd called on EPVM object: %p -> %p", object, to);
}

mps_addr_t epvmisfwd(mps_addr_t object) {
 error("isfwd called on EPVM object: %p", object);
 return NULL;
}

void epvmcopy(mps_addr_t object, mps_addr_t to) {
 error("copy called on EPVM object: %p -> %p", object, to);
}

void epvmpad(mps_addr_t base, size_t size) {
 error("pad called in EPVM: %p, %u", base, size);
}

struct mps_fmt_A_s fmtepvm =
{
 (mps_align_t) 8,
 &epvmscan,
 &epvmskip,
 &epvmcopy,
 &epvmfwd,
 &epvmisfwd,
 &epvmpad
};

/* in the following, size is the number of words you want
   to allocate
*/

psobj *allocepvm(mps_ap_t ap, int size) {
 psobj *a;
 die(allocrepvm(&a, ap, size), "Reserve: ");
 return a;
}

mps_res_t allocrepvm(psobj **q, mps_ap_t ap, int size) {
 mps_addr_t p;
 int i;
 size_t bytes;
 mps_res_t res;

 asserts(sizeof(struct psobj) == 8, "Aaarg! How can EPVM pools possibly work");
 bytes = size*8;

 asserts(size > 0, "allocepvm with zero size");

 do
 {
  res = mps_reserve(&p, ap, bytes);
  if (res != MPS_RES_OK) {
   return res;
  }
  *q=p;

  for(i=0; i<size; i+=1)
  {
   (*q+i)->obj  = NULL;
   (*q+i)->size = 0;
  }
 }
 while (!mps_commit(ap, p, bytes));
 commentif(alloccomments, "allocated %p.", q);

 return MPS_RES_OK;
}

void splatepvm(psobj *obj) {
 obj->size = 0;
}
 
mps_bool_t issplatepvm(psobj *obj) {
 return (obj->size == 0);
}

static mps_res_t epvmscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 unsigned int i;

 MPS_SCAN_BEGIN(ss)
 {
  while (base < limit)
  {
   psobj *obj = base;
   mps_res_t res;
   mps_addr_t p;

   commentif(alloccomments, "scan %p", base);

   asserts(obj->size > 0, "scan on splatted object at %p", obj);

   p = obj->obj;
   asserts(p != NULL, "NULL pointer in EPVM obj at %p", obj);
   res = MPS_FIX(ss, (mps_addr_t *) &p); /* A ghastly PUN! */
   if (res != MPS_RES_OK) return res;
   asserts(p != NULL, "reference in EPVM fixed to NULL at %p", obj);
   obj->obj = p;

   for (i=1; i<obj->size; i++) {
    p = obj->obj + i;
    res = MPS_FIX(ss, (mps_addr_t *) &p);
    if (res != MPS_RES_OK) return res;
    asserts(p == obj->obj+i, "reference in EPVM changed at %p", obj);
   }
   base = (char *) base + 8;
  }
 }
 MPS_SCAN_END(ss);
 return MPS_RES_OK;
}

